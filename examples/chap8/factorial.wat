(module 
  (import "env" "greet" (func $print_int (param $number i32)))
(type $funi->i(func (param $fact i32)(param $x i32) (result i32)))
(type $alloc_sig(func (param $size i32) (result i32)))
(type $entry_point(func  ))(func $__cml_main (type $entry_point) 
(local $fact i32)(local $pair_addr i32)(local $closure_ptr i32)(local $__fptr i32)
i32.const 4
call $alloc
local.tee $closure_ptr
i32.const 0
local.set $__fptr
local.get $__fptr
i32.store offset=0 align=4
local.get $closure_ptr
local.set $fact
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
)(table $funi->i 1 funcref)(memory $linear_memory 1 2)(global $alloc_ptr(mut i32 ) i32.const 0
)(start $__cml_main)(elem  (table $funi->i)(offset i32.const 0
)funcref (item ref.func $funi->i0
)
))