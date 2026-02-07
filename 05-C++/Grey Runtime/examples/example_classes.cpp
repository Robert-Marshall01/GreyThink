// Grey Runtime - Example: Classes & Objects
// Demonstrates OOP features like Python:
//   class Point:
//       def __init__(self, x, y):
//           self.x = x
//           self.y = y
//       def distance(self):
//           return sqrt(self.x**2 + self.y**2)
//
// Build: cmake --build build && build/grey_examples

#include "grey/runtime.h"
#include "grey/bytecode.h"
#include "grey/vm.h"
#include "grey/objects.h"
#include "grey/memory.h"

#include <iostream>
#include <cmath>

using namespace grey;

int main() {
    std::cout << "=== Grey Runtime: Classes & Objects ===" << std::endl;
    std::cout << std::endl;

    VMConfig config;
    VM vm(config);

    // -----------------------------------------------
    // Example 1: Direct object manipulation
    //   Manually create an instance, set fields, read them back
    // -----------------------------------------------
    {
        std::cout << "--- Direct Object Manipulation ---" << std::endl;

        // Create a class
        auto* class_name = new ObjString("Point");
        vm.gc().track(class_name);
        auto* klass = new ObjClass(class_name->value);
        vm.gc().track(klass);

        // Create an instance
        auto* inst = new ObjInstance(klass);
        vm.gc().track(inst);

        // Set fields x=3, y=4
        auto* field_x = new ObjString("x");
        auto* field_y = new ObjString("y");
        vm.gc().track(field_x);
        vm.gc().track(field_y);

        inst->fields.set(Value::object(field_x), Value::integer(3));
        inst->fields.set(Value::object(field_y), Value::integer(4));

        // Read them back
        Value x_val, y_val;
        inst->fields.get(Value::object(field_x), x_val);
        inst->fields.get(Value::object(field_y), y_val);

        std::cout << "Point instance:" << std::endl;
        std::cout << "  x = " << x_val.as_int() << std::endl;
        std::cout << "  y = " << y_val.as_int() << std::endl;

        // Compute distance = sqrt(x*x + y*y)
        f64 x = static_cast<f64>(x_val.as_int());
        f64 y = static_cast<f64>(y_val.as_int());
        f64 distance = std::sqrt(x * x + y * y);
        std::cout << "  distance = " << distance << std::endl;
        std::cout << "  Expected: 5.0" << std::endl;
    }

    // -----------------------------------------------
    // Example 2: Class with method via native
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Class with Native Method ---" << std::endl;

        // Register a native that creates a Rectangle
        vm.define_native("make_rect", [&vm](int argc, Value* args) -> Value {
            if (argc < 2) return Value::nil();

            auto* klass = new ObjClass("Rectangle");
            vm.gc().track(klass);
            auto* inst = new ObjInstance(klass);
            vm.gc().track(inst);

            auto* w_key = new ObjString("width");
            auto* h_key = new ObjString("height");
            vm.gc().track(w_key);
            vm.gc().track(h_key);

            inst->fields.set(Value::object(w_key), args[0]);
            inst->fields.set(Value::object(h_key), args[1]);
            return Value::object(inst);
        }, 2);

        // Register area() native
        // NOTE: ObjMap uses Value::operator== which compares raw bits (pointer address).
        // We must search entries manually by string value for reliable field lookup.
        vm.define_native("rect_area", [](int argc, Value* args) -> Value {
            if (argc < 1 || !args[0].is_object()) return Value::nil();
            auto* inst = static_cast<ObjInstance*>(args[0].as_object<ObjHeader>());
            if (!inst || inst->type != ObjType::Instance) return Value::nil();

            // Linear scan for fields by string value (pointer-independent)
            Value w = Value::nil(), h = Value::nil();
            for (auto& entry : inst->fields.entries) {
                if (!entry.occupied || !entry.key.is_object()) continue;
                auto* key_hdr = entry.key.as_object<ObjHeader>();
                if (key_hdr->type != ObjType::String) continue;
                auto* key_str = static_cast<ObjString*>(key_hdr);
                if (key_str->value == "width")  w = entry.value;
                if (key_str->value == "height") h = entry.value;
            }

            if (w.is_int() && h.is_int()) {
                return Value::integer(w.as_int() * h.as_int());
            }
            return Value::nil();
        }, 1);

        // Use via bytecode: rect = make_rect(5, 10); area = rect_area(rect)
        Chunk chunk;

        // make_rect(5, 10)
        auto* mr_name = new ObjString("make_rect");
        vm.gc().track(mr_name);
        u16 mr_idx = chunk.add_constant(Value::object(mr_name));
        chunk.emit(OpCode::LOAD_GLOBAL, 1);
        chunk.emit_u16(mr_idx, 1);
        chunk.emit_constant(Value::integer(5), 1);
        chunk.emit_constant(Value::integer(10), 1);
        chunk.emit(OpCode::CALL, 1);
        chunk.emit(static_cast<u8>(2), 1);

        // rect_area(rect)
        auto* ra_name = new ObjString("rect_area");
        vm.gc().track(ra_name);
        u16 ra_idx = chunk.add_constant(Value::object(ra_name));
        chunk.emit(OpCode::LOAD_GLOBAL, 2);
        chunk.emit_u16(ra_idx, 2);
        chunk.emit(OpCode::SWAP, 2);   // swap so rect is the arg
        chunk.emit(OpCode::CALL, 2);
        chunk.emit(static_cast<u8>(1), 2);

        chunk.emit(OpCode::DEBUG_PRINT, 3);
        chunk.emit(OpCode::RETURN, 4);

        auto* fn = new ObjFunction();
        fn->name = "class_demo";
        fn->chunk = chunk;
        fn->arity = 0;
        vm.gc().track(fn);

        Value result = vm.execute(fn);
        std::cout << "rect_area(make_rect(5, 10)) = ";
        if (result.is_int()) std::cout << result.as_int();
        std::cout << std::endl;
        std::cout << "Expected: 50" << std::endl;
    }

    // -----------------------------------------------
    // Example 3: Inheritance check
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Inheritance (isinstance) ---" << std::endl;

        auto* animal = new ObjClass("Animal");
        auto* dog = new ObjClass("Dog");
        vm.gc().track(animal);
        vm.gc().track(dog);

        // Dog extends Animal
        dog->superclass = animal;

        auto* fido = new ObjInstance(dog);
        vm.gc().track(fido);

        // Check: fido instanceof Dog? (should be true)
        ObjClass* k = fido->klass;
        bool is_dog = false;
        while (k) {
            if (k == dog) { is_dog = true; break; }
            k = k->superclass;
        }
        std::cout << "fido instanceof Dog: " << (is_dog ? "true" : "false") << std::endl;

        // Check: fido instanceof Animal? (should be true via inheritance)
        k = fido->klass;
        bool is_animal = false;
        while (k) {
            if (k == animal) { is_animal = true; break; }
            k = k->superclass;
        }
        std::cout << "fido instanceof Animal: " << (is_animal ? "true" : "false") << std::endl;
        std::cout << "Expected: true, true" << std::endl;
    }

    std::cout << std::endl;
    std::cout << "=== Classes & Objects Complete ===" << std::endl;
    return 0;
}
