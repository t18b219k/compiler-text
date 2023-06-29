(module (type $entry_point(func  ))(func $__cml_main (type $entry_point)
(local $jk i32)(local $pair_addr i32)(local $closure_ptr i32)(local $__fptr i32)
i32.const 1
if (result i32)
i32.const 123
else
i32.const 456
end
i32.const 234
i32.const 567
i32.eq
if (result i32)
i32.const 98
else
i32.const 76
end
i32.const 8
call $alloc
local.tee $pair_addr
i32.store offset=4 align=4
local.get $pair_addr
i32.store offset=0 align=4
local.get $pair_addr
local.set $jk
)(start $__cml_main))