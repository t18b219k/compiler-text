(module (type $alloc_sig(func (param $size i32) (result i32)))(type $entry_point(func  ))(func $__cml_main (type $entry_point) 
(local $it i32)(local $pair_addr i32)(local $closure_ptr i32)(local $__fptr i32)
i32.const 8
call $alloc
i32.const 123
i32.store offset=0 align=4
local.get $pair_addr
i32.const 456
i32.store offset=4 align=4
local.get $pair_addr
local.set $it
)(func $alloc (type $alloc_sig)(param $size i32) (result i32)
(local $old i32)
global.get $alloc_ptr
local.set $old
local.get $old
local.get $size
i32.add
global.set $alloc_ptr
local.get $old
)(memory $linear_memory 1 2)(global $alloc_ptr(mut i32 ) i32.const 0
)(start $__cml_main))
