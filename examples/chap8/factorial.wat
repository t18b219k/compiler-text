(module (type $funi->i(func (param $fact i32)(param $x i32) (result i32)))(type $alloc_sig(func (param $size i32) (result i32)))(type $entry_point(func  ))(type $print_int(func (param $value i32) ))(type $print_bool(func (param $value i32) ))(type $print_string(func (param $value i32) ))(type $print_token(func  ))(import "env" "linear_memory" (memory $linear_memory 2))(import "env" "print_int" (func $print_int (type $print_int)))(import "env" "print_bool" (func $print_bool (type $print_bool)))(import "env" "print_string" (func $print_string (type $print_string)))(import "env" "print_lparen" (func $print_lparen (type $print_token)))(import "env" "print_rparen" (func $print_rparen (type $print_token)))(import "env" "print_val" (func $print_val (type $print_token)))(import "env" "print_comma" (func $print_comma (type $print_token)))(import "env" "print_equal" (func $print_equal (type $print_token)))(import "env" "print_colon" (func $print_colon (type $print_token)))(import "env" "print_arrow" (func $print_arrow (type $print_token)))(import "env" "print_space" (func $print_space (type $print_token)))(func $__cml_main (type $entry_point) 
(local $it i32)(local $fact i32)(local $pair_addr i32)(local $closure_ptr i32)(local $__fptr i32)
i32.const 4
global.set $alloc_ptr
i32.const 4
call $alloc
local.tee $closure_ptr
i32.const 0
local.set $__fptr
local.get $__fptr
i32.store offset=0 align=4
local.get $closure_ptr
local.set $fact
local.get $fact
local.tee $__fptr
i32.const 10
local.get $__fptr
i32.load offset=0 align=4
call_indirect $funi->i (type $funi->i)
local.set $it
call $print_val
call $print_space
i32.const 0
call $print_string
call $print_equal
local.get $it
call $print_int
)(func $alloc (type $alloc_sig)(param $size i32) (result i32)
(local $old i32)
global.get $alloc_ptr
local.set $old
local.get $old
local.get $size
i32.add
global.set $alloc_ptr
local.get $old
)(func $funi->i0 (type $funi->i)(param $fact i32)(param $x i32) (result i32)
(local $pair_addr i32)(local $closure_ptr i32)(local $__fptr i32)
local.get $fact
i32.load offset=0 align=4
local.set $__fptr
local.get $x
i32.const 0
i32.eq
if (result i32)
i32.const 1
else
local.get $x
i32.const 1
i32.eq
if (result i32)
i32.const 1
else
local.get $x
local.get $fact
local.tee $__fptr
local.get $x
i32.const 1
i32.sub
local.get $__fptr
i32.load offset=0 align=4
call_indirect $funi->i (type $funi->i)
i32.mul
end
end
)(table $funi->i 1 funcref)(global $alloc_ptr(mut i32 ) i32.const 0
)(start $__cml_main)(elem  (table $funi->i)(offset i32.const 0
)funcref 
(item ref.func $funi->i0
)
))
