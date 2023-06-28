(module (type $entry_point(func  ))(func $__cml_main (type $entry_point) 
(local $ju i32)(local $it i32)
i32.const 1
i32.const 2
i32.const 8
call $alloc
local.tee $pair_addr
i32.store offset=4 align=4
local.get $pair_addr
i32.store offset=0 align=4
local.get $pair_addr
local.set $it
)(start $__cml_main))
