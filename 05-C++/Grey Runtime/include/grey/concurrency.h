// Grey Runtime - Concurrency System
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"
#include "grey/memory.h"
#include "grey/objects.h"

#include <thread>
#include <mutex>
#include <shared_mutex>
#include <condition_variable>
#include <queue>
#include <future>
#include <atomic>
#include <functional>
#include <deque>

namespace grey {

// ============================================================
// 3.2 Synchronization Primitives
// ============================================================

// ----- Mutex -----
class GreyMutex {
public:
    void lock()     { mtx_.lock(); }
    void unlock()   { mtx_.unlock(); }
    bool try_lock() { return mtx_.try_lock(); }
private:
    std::mutex mtx_;
};

// ----- RWLock -----
class GreyRWLock {
public:
    void read_lock()     { mtx_.lock_shared(); }
    void read_unlock()   { mtx_.unlock_shared(); }
    void write_lock()    { mtx_.lock(); }
    void write_unlock()  { mtx_.unlock(); }
    bool try_read_lock() { return mtx_.try_lock_shared(); }
    bool try_write_lock(){ return mtx_.try_lock(); }
private:
    std::shared_mutex mtx_;
};

// ----- Atomic Value -----
class AtomicValue {
public:
    AtomicValue() : bits_(Value::nil().bits) {}
    explicit AtomicValue(Value v) : bits_(v.bits) {}

    Value load() const {
        Value v;
        v.bits = bits_.load(std::memory_order_acquire);
        return v;
    }
    void store(Value v) {
        bits_.store(v.bits, std::memory_order_release);
    }
    bool compare_exchange(Value& expected, Value desired) {
        return bits_.compare_exchange_strong(
            expected.bits, desired.bits,
            std::memory_order_acq_rel);
    }
private:
    std::atomic<u64> bits_;
};

// ============================================================
// 3.1 Fiber (Green threads / cooperative multitasking)
// ============================================================

enum class FiberState : u8 {
    Created,
    Running,
    Suspended,
    Waiting,
    Completed,
    Failed,
};

struct ObjFiber : ObjHeader {
    FiberState state = FiberState::Created;
    ObjClosure* entry = nullptr;
    Value result = Value::nil();

    // Each fiber has its own stack and call frames
    ValueStack stack;
    CallFrame  frames[256];
    u32        frame_count = 0;

    // Parent fiber (for yield-to semantics)
    ObjFiber* caller = nullptr;

    // Error
    Value error = Value::nil();

    ObjFiber() : ObjHeader(ObjType::Fiber) {}
    explicit ObjFiber(ObjClosure* fn)
        : ObjHeader(ObjType::Fiber), entry(fn) {}
};

// ============================================================
// 3.2 Channel (CSP-style message passing)
// ============================================================

struct ObjChannel : ObjHeader {
    std::queue<Value> buffer;
    size_t capacity;
    bool closed = false;

    std::mutex mtx;
    std::condition_variable not_empty;
    std::condition_variable not_full;

    explicit ObjChannel(size_t cap = 0)
        : ObjHeader(ObjType::Channel), capacity(cap) {}

    bool send(Value val) {
        std::unique_lock<std::mutex> lock(mtx);
        if (closed) return false;
        if (capacity > 0) {
            not_full.wait(lock, [&] { return buffer.size() < capacity || closed; });
            if (closed) return false;
        }
        buffer.push(val);
        not_empty.notify_one();
        return true;
    }

    bool receive(Value& out) {
        std::unique_lock<std::mutex> lock(mtx);
        not_empty.wait(lock, [&] { return !buffer.empty() || closed; });
        if (buffer.empty()) return false;
        out = buffer.front();
        buffer.pop();
        not_full.notify_one();
        return true;
    }

    void close() {
        std::lock_guard<std::mutex> lock(mtx);
        closed = true;
        not_empty.notify_all();
        not_full.notify_all();
    }

    bool is_closed() const { return closed; }
    size_t size() const { return buffer.size(); }
};

// ============================================================
// 3.3 Future / Promise
// ============================================================

enum class FutureState : u8 {
    Pending,
    Resolved,
    Rejected,
};

struct ObjFuture : ObjHeader {
    FutureState state = FutureState::Pending;
    Value result = Value::nil();
    Value error  = Value::nil();

    std::mutex mtx;
    std::condition_variable cv;
    std::vector<std::function<void(Value)>> on_resolve;
    std::vector<std::function<void(Value)>> on_reject;

    // Fiber waiting on this future
    ObjFiber* waiting_fiber = nullptr;

    ObjFuture() : ObjHeader(ObjType::Future) {}

    void resolve(Value val) {
        std::lock_guard<std::mutex> lock(mtx);
        if (state != FutureState::Pending) return;
        state = FutureState::Resolved;
        result = val;
        for (auto& cb : on_resolve) cb(val);
        on_resolve.clear();
        cv.notify_all();
    }

    void reject(Value err) {
        std::lock_guard<std::mutex> lock(mtx);
        if (state != FutureState::Pending) return;
        state = FutureState::Rejected;
        error = err;
        for (auto& cb : on_reject) cb(err);
        on_reject.clear();
        cv.notify_all();
    }

    Value await() {
        std::unique_lock<std::mutex> lock(mtx);
        cv.wait(lock, [&] { return state != FutureState::Pending; });
        if (state == FutureState::Rejected) {
            throw RuntimeError(ErrorCode::RuntimeError, value_to_string(error));
        }
        return result;
    }

    bool is_pending()  const { return state == FutureState::Pending; }
    bool is_resolved() const { return state == FutureState::Resolved; }
    bool is_rejected() const { return state == FutureState::Rejected; }
};

// ============================================================
// 3.3 Async Runtime (Event Loop + Task Scheduler)
// ============================================================

class AsyncRuntime {
public:
    using Task = std::function<void()>;

    AsyncRuntime(size_t num_workers = 0)
        : running_(false) {
        if (num_workers == 0) {
            num_workers = std::max(1u, std::thread::hardware_concurrency());
        }
        num_workers_ = num_workers;
    }

    ~AsyncRuntime() { stop(); }

    void start() {
        running_ = true;
        for (size_t i = 0; i < num_workers_; i++) {
            workers_.emplace_back([this] { worker_loop(); });
        }
    }

    void stop() {
        {
            std::lock_guard<std::mutex> lock(mtx_);
            running_ = false;
        }
        cv_.notify_all();
        for (auto& t : workers_) {
            if (t.joinable()) t.join();
        }
        workers_.clear();
    }

    // Schedule a task
    void spawn(Task task) {
        {
            std::lock_guard<std::mutex> lock(mtx_);
            tasks_.push_back(std::move(task));
        }
        cv_.notify_one();
    }

    // Spawn and return a future
    ObjFuture* spawn_future(std::function<Value()> work, GarbageCollector& gc) {
        auto* future = new ObjFuture();
        gc.track(future);
        spawn([future, work = std::move(work)] {
            try {
                Value result = work();
                future->resolve(result);
            } catch (const RuntimeError& e) {
                // Can't make_string here without GC access, store nil
                future->reject(Value::nil());
            }
        });
        return future;
    }

    // Process pending tasks on current thread (event loop tick)
    void poll() {
        Task task;
        {
            std::lock_guard<std::mutex> lock(mtx_);
            if (tasks_.empty()) return;
            task = std::move(tasks_.front());
            tasks_.pop_front();
        }
        if (task) task();
    }

    bool has_pending() const {
        std::lock_guard<std::mutex> lock(mtx_);
        return !tasks_.empty();
    }

    size_t pending_count() const {
        std::lock_guard<std::mutex> lock(mtx_);
        return tasks_.size();
    }

private:
    void worker_loop() {
        while (true) {
            Task task;
            {
                std::unique_lock<std::mutex> lock(mtx_);
                cv_.wait(lock, [&] { return !running_ || !tasks_.empty(); });
                if (!running_ && tasks_.empty()) return;
                task = std::move(tasks_.front());
                tasks_.pop_front();
            }
            if (task) task();
        }
    }

    size_t num_workers_;
    std::vector<std::thread> workers_;
    std::deque<Task> tasks_;
    mutable std::mutex mtx_;
    std::condition_variable cv_;
    std::atomic<bool> running_;

    // Fiber scheduling
public:
    void schedule_fiber(ObjFiber* fiber) {
        ready_fibers_.push_back(fiber);
    }

    ObjFiber* next_fiber() {
        if (ready_fibers_.empty()) return nullptr;
        ObjFiber* f = ready_fibers_.front();
        ready_fibers_.pop_front();
        return f;
    }

private:
    std::deque<ObjFiber*> ready_fibers_;
};

} // namespace grey
