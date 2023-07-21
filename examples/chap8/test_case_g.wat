;; compiled from fun g h = fn x => add(h x ,h 123);
(module
        (type $function(func (param $g i32)(param $h i32) (result i32) ))
        (type $closure_call_sig(func (param $closure i32)(param $arg i32) (result i32) ))
        (type $alloc_sig(func (param $size i32) (result i32) ))
        (type $entry_point(func))
        (type $print_int(func (param $value i32)))
        (type $print_bool(func (param $value i32)))
        (type $print_string(func (param $ptr i32)(param $size i32)))
        (type $print_token(func))
        (import "env" "linear_memory" (memory $linear_memory 2))
        (import "env" "print_int" (func $print_int (type $print_int)))
        (import "env" "print_bool" (func $print_bool (type $print_bool)))
        (import "env" "print_string" (func $print_string (type $print_string)))
        (import "env" "print_lparen" (func $print_lparen (type $print_token)))
        (import "env" "print_rparen" (func $print_rparen (type $print_token)))
        (import "env" "print_val" (func $print_val (type $print_token)))
        (import "env" "print_comma" (func $print_comma (type $print_token)))
        (import "env" "print_equal" (func $print_equal (type $print_token)))
        (import "env" "print_colon" (func $print_colon (type $print_token)))
        (import "env" "print_arrow" (func $print_arrow (type $print_token)))
        (import "env" "print_space" (func $print_space (type $print_token)))
        (import "env" "print_double_quote" (func $print_double_quote (type $print_token)))
        (import "env" "print_new_line" (func $print_new_line (type $print_token)))
        (func $__cml_main (type $entry_point) 
                (local $g i32)
                (local $str_ptr i32)
                (local $closure_ptr i32)
                i32.const 32
                global.set $alloc_ptr
                i32.const 4
                call $alloc
                local.tee $closure_ptr
                i32.const 0
                i32.store offset=0 align=4
                local.get $closure_ptr
                local.set $g
                call $print_val
                call $print_space
                i32.const 0
                i32.const 1
                call $print_string
                call $print_colon
                i32.const 1
                i32.const 30
                call $print_string
                call $print_new_line
)
        (func $closure_call (type $closure_call_sig)(param $closure i32)(param $arg i32) (result i32)
                local.get $closure
                local.get $arg
                local.get $closure
                i32.load offset=0 align=4
                call_indirect $function (type $function)
)
        (func $alloc (type $alloc_sig)(param $size i32) (result i32)
                (local $old i32)
                global.get $alloc_ptr
                local.set $old
                local.get $old
                local.get $size
                i32.add
                global.set $alloc_ptr
                local.get $old
)
        (func $function0 (type $function)(param $g i32)(param $h i32) (result i32)
                (local $str_ptr i32)
                (local $closure_ptr i32)
                i32.const 8
                call $alloc
                local.tee $closure_ptr
                i32.const 0;; バグ　ここは1になるはず. そうでなければ新しくクロージャを作って返せていない。
                i32.store offset=0 align=4
                local.get $closure_ptr
                local.get $h
                i32.store offset=4 align=4
                local.get $closure_ptr
)
        (func $function1 (type $function)(param $non_recursive_closure i32)(param $x i32) (result i32)
                (local $h i32)
                (local $str_ptr i32)
                (local $closure_ptr i32)
                local.get $non_recursive_closure
                i32.load offset=4 align=4　;; ここにはh が入っているので問題なし.
                local.set $h
                local.get $h
                local.get $x
                call $closure_call
                local.get $h
                i32.const 123
                call $closure_call
                i32.add
)
        (table $function 2 funcref)
        (global $alloc_ptr (mut i32) i32.const 0)
        (export "__cml_main" (func $__cml_main))
        (start $__cml_main)     (elem (table $function)(offset i32.const 0)funcref (item ref.func $function0)
(item ref.func $function1)
)
        (data $linear_memory(offset i32.const 1) "((int -> int) -> (int -> int))")
        (data $linear_memory(offset i32.const 0) "g")
)