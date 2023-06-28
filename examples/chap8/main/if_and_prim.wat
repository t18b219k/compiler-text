(module (type $entry_point(func  ))(func $__cml_main (type $entry_point) 
(local $ju i32)(local $it i32)
i32.const 2
i32.const 3
i32.eq
if (result i32)
i32.const 2
i32.const 3
i32.add
else
i32.const 2
i32.const 3
i32.sub
end
local.set $it
i32.const 1
i32.const 23
i32.add
local.set $ju
)(start $__cml_main))