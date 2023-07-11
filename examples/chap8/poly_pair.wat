;;Polymorphic function pair and applycations
;; compiled from 
;; val it = (fn x=>(x,x),fn x=>x);
;; val h = #2 it 12345;
;; val j = #1 it 12345;
(module
	(type $funi->i*i(func (param $non_recursive_closure i32)(param $x i32) (result i32) ))
	(type $funi->i(func (param $non_recursive_closure i32)(param $x i32) (result i32) ))
	(type $pair_print_<?a-><?a*?a>*?b->?b>(func (param $__pair_addr i32)))
	(type $pair_print_<i*i>(func (param $__pair_addr i32)))
	(type $alloc_sig(func (param $size i32) (result i32) ))
	(type $pair_con_gc_i32_gc_i32(func (param $e1 i32)(param $e2 i32) (result i32) ))
	(type $pair_con_nogc_i32_nogc_i32(func (param $e1 i32)(param $e2 i32) (result i32) ))
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
	(func $pair_print_<?a-><?a*?a>*?b->?b> (type $pair_print_<?a-><?a*?a>*?b->?b>)
	(local $str_ptr i32)
	call $print_lparen
	local.get 0
	i32.load offset=0 align=4
	drop
	i32.const 2
	i32.const 17
	call $print_string
	call $print_comma
	local.get 0
	i32.load offset=4 align=4
	drop
	i32.const 19
	i32.const 10
	call $print_string
	call $print_rparen
)
	(func $pair_print_<i*i> (type $pair_print_<i*i>)
	(local $str_ptr i32)
	call $print_lparen
	local.get 0
	i32.load offset=0 align=4
	call $print_int
	call $print_comma
	local.get 0
	i32.load offset=4 align=4
	call $print_int
	call $print_rparen
)
	(func $__cml_main (type $entry_point) 
	(local $j i32)
	(local $h i32)
	(local $it i32)
	(local $str_ptr i32)
	(local $closure_ptr i32)
	(local $__fptr i32)
	i32.const 80
	global.set $alloc_ptr
	i32.const 4
	call $alloc
	local.tee $closure_ptr
	i32.const 0
	local.set $__fptr
	local.get $__fptr
	i32.store offset=0 align=4
	local.get $closure_ptr
	i32.const 4
	call $alloc
	local.tee $closure_ptr
	i32.const 0
	local.set $__fptr
	local.get $__fptr
	i32.store offset=0 align=4
	local.get $closure_ptr
	call $pair_con_gc_i32_gc_i32
	local.set $it
	call $print_val
	call $print_space
	i32.const 0
	i32.const 2
	call $print_string
	call $print_equal
	local.get $it
	call $pair_print_<?a-><?a*?a>*?b->?b>
	call $print_colon
	i32.const 29
	i32.const 32
	call $print_string
	call $print_new_line
	local.get $it
	i32.load offset=4 align=4
	local.tee $__fptr
	i32.const 12345
	local.get $__fptr
	i32.load offset=0 align=4
	call_indirect $funi->i (type $funi->i)
	local.set $h
	call $print_val
	call $print_space
	i32.const 61
	i32.const 1
	call $print_string
	call $print_equal
	local.get $h
	call $print_int
	call $print_colon
	i32.const 62
	i32.const 3
	call $print_string
	call $print_new_line
	local.get $it
	i32.load offset=0 align=4
	local.tee $__fptr
	i32.const 12345
	local.get $__fptr
	i32.load offset=0 align=4
	call_indirect $funi->i*i (type $funi->i*i)
	local.set $j
	call $print_val
	call $print_space
	i32.const 65
	i32.const 1
	call $print_string
	call $print_equal
	local.get $j
	call $pair_print_<i*i>
	call $print_colon
	i32.const 66
	i32.const 11
	call $print_string
	call $print_new_line
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
	(func $pair_con_gc_i32_gc_i32 (type $pair_con_gc_i32_gc_i32)
	(local $pair_addr i32)
	i32.const 8
	call $alloc
	local.tee $pair_addr
	local.get 0
	i32.store offset=0 align=4
	local.get $pair_addr
	local.get 1
	i32.store offset=4 align=4
	local.get $pair_addr
)
	(func $pair_con_nogc_i32_nogc_i32 (type $pair_con_nogc_i32_nogc_i32)
	(local $pair_addr i32)
	i32.const 8
	call $alloc
	local.tee $pair_addr
	local.get 0
	i32.store offset=0 align=4
	local.get $pair_addr
	local.get 1
	i32.store offset=4 align=4
	local.get $pair_addr
)
	(func $funi->i*i0 (type $funi->i*i)(param $non_recursive_closure i32)(param $x i32) (result i32)
	(local $str_ptr i32)
	(local $closure_ptr i32)
	(local $__fptr i32)
	local.get $non_recursive_closure
	i32.load offset=0 align=4
	local.set $__fptr
	local.get $x
	local.get $x
	call $pair_con_nogc_i32_nogc_i32
)
	(func $funi->i0 (type $funi->i)(param $non_recursive_closure i32)(param $x i32) (result i32)
	(local $str_ptr i32)
	(local $closure_ptr i32)
	(local $__fptr i32)
	local.get $non_recursive_closure
	i32.load offset=0 align=4
	local.set $__fptr
	local.get $x
)
	(table $funi->i*i 1 funcref)
	(table $funi->i 1 funcref)
	(global $alloc_ptr (mut i32) i32.const 0)
	(export "__cml_main" (func $__cml_main))
	(start $__cml_main)	(elem (table $funi->i*i)(offset i32.const 0)funcref (item ref.func $funi->i*i0)
)
	(elem (table $funi->i)(offset i32.const 0)funcref (item ref.func $funi->i0)
)
	(data $linear_memory(offset i32.const 66) "(int * int)")
	(data $linear_memory(offset i32.const 65) "j")
	(data $linear_memory(offset i32.const 62) "int")
	(data $linear_memory(offset i32.const 61) "h")
	(data $linear_memory(offset i32.const 29) "(('a -> ('a * 'a)) * ('b -> 'b))")
	(data $linear_memory(offset i32.const 19) "('b -> 'b)")
	(data $linear_memory(offset i32.const 2) "('a -> ('a * 'a))")
	(data $linear_memory(offset i32.const 0) "it")
)