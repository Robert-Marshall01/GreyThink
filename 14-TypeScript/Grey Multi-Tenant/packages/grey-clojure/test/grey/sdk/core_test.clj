(ns grey.sdk.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [grey.sdk.error :as error]
            [grey.sdk.result :as r]
            [grey.sdk.options :as opts]))

;; =============================================================================
;; Error Tests
;; =============================================================================

(deftest grey-error-test
  (testing "creates error with code and message"
    (let [err (error/grey-error :not-found "Resource not found")]
      (is (= :not-found (:code err)))
      (is (= "Resource not found" (:message err)))
      (is (nil? (:details err)))))
  
  (testing "creates error with details"
    (let [err (error/grey-error :validation-error "Invalid" {:field "email"})]
      (is (= {:field "email"} (:details err)))))
  
  (testing "unknown code defaults to :unknown"
    (let [err (error/grey-error :invalid-code "test")]
      (is (= :unknown (:code err))))))

(deftest error-constructors-test
  (testing "unauthorized"
    (is (= :unauthorized (:code (error/unauthorized)))))
  
  (testing "validation-error"
    (is (= :validation-error (:code (error/validation-error "test")))))
  
  (testing "not-found"
    (is (= :not-found (:code (error/not-found))))))

(deftest from-any-test
  (testing "passthrough grey error"
    (let [err (error/unauthorized)]
      (is (= err (error/from-any err)))))
  
  (testing "keyword to error"
    (let [err (error/from-any :timeout)]
      (is (= :timeout (:code err)))))
  
  (testing "string to error"
    (let [err (error/from-any "Something went wrong")]
      (is (= :unknown (:code err)))
      (is (= "Something went wrong" (:message err)))))
  
  (testing "exception to error"
    (let [err (error/from-any (Exception. "test error"))]
      (is (= :unknown (:code err)))
      (is (= "test error" (:message err))))))

;; =============================================================================
;; Result Tests
;; =============================================================================

(deftest result-creation-test
  (testing "ok creates success result"
    (let [result (r/ok {:id 1})]
      (is (true? (:ok result)))
      (is (= {:id 1} (:data result)))))
  
  (testing "err creates error result"
    (let [result (r/err (error/unauthorized))]
      (is (false? (:ok result)))
      (is (= :unauthorized (get-in result [:error :code]))))))

(deftest result-predicates-test
  (testing "ok?"
    (is (true? (r/ok? (r/ok nil))))
    (is (false? (r/ok? (r/err :test)))))
  
  (testing "err?"
    (is (true? (r/err? (r/err :test))))
    (is (false? (r/err? (r/ok nil))))))

(deftest result-then-test
  (testing "then applies f on ok"
    (let [result (-> (r/ok 5)
                     (r/then (fn [x] (r/ok (* x 2)))))]
      (is (r/ok? result))
      (is (= 10 (:data result)))))
  
  (testing "then skips on err"
    (let [err (error/unauthorized)
          result (-> (r/err err)
                     (r/then (fn [_] (r/ok "should not run"))))]
      (is (r/err? result))
      (is (= :unauthorized (get-in result [:error :code]))))))

(deftest result-unwrap-test
  (testing "unwrap returns data on ok"
    (is (= {:id 1} (r/unwrap (r/ok {:id 1})))))
  
  (testing "unwrap throws on err"
    (is (thrown? clojure.lang.ExceptionInfo
                 (r/unwrap (r/err (error/unauthorized))))))
  
  (testing "unwrap-or returns default on err"
    (is (= :default (r/unwrap-or (r/err :test) :default)))))

(deftest result-combine-test
  (testing "combines all ok results"
    (let [result (r/combine (r/ok 1) (r/ok 2) (r/ok 3))]
      (is (r/ok? result))
      (is (= [1 2 3] (:data result)))))
  
  (testing "returns first error"
    (let [result (r/combine (r/ok 1) (r/err (error/unauthorized)) (r/ok 3))]
      (is (r/err? result))
      (is (= :unauthorized (get-in result [:error :code]))))))

;; =============================================================================
;; Options Tests
;; =============================================================================

(deftest options-creation-test
  (testing "creates options with all fields"
    (let [o (opts/options "localhost" 8080 false)]
      (is (= "localhost" (:host o)))
      (is (= 8080 (:port o)))
      (is (false? (:use-tls o)))
      (is (= 30000 (:timeout-ms o)))))
  
  (testing "local creates localhost options"
    (let [o (opts/local 9000)]
      (is (= "localhost" (:host o)))
      (is (= 9000 (:port o)))
      (is (false? (:use-tls o)))))
  
  (testing "production creates TLS options"
    (let [o (opts/production "api.grey.com")]
      (is (= "api.grey.com" (:host o)))
      (is (= 443 (:port o)))
      (is (true? (:use-tls o))))))

(deftest options-endpoint-test
  (testing "formats endpoint correctly"
    (is (= "localhost:8080" (opts/endpoint (opts/local 8080))))
    (is (= "api.grey.com:443" (opts/endpoint (opts/production "api.grey.com"))))))

(deftest options-modifiers-test
  (testing "with-timeout updates timeout"
    (let [o (-> (opts/local)
                (opts/with-timeout 5000))]
      (is (= 5000 (:timeout-ms o)))))
  
  (testing "with-metadata merges metadata"
    (let [o (-> (opts/local)
                (opts/with-metadata {:key "value"}))]
      (is (= {:key "value"} (:metadata o))))))
