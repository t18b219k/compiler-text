use std::{
    ffi::{c_char, CStr},
    io::Write,
};

use log::{error, info, warn};
use wasmer::{
    Function, FunctionEnv, FunctionEnvMut, Imports, Instance, Memory, MemoryType, Module, Store,
};
#[no_mangle]
extern "C" fn start_logger() {
    fern::Dispatch::new()
        .chain(
            fern::Dispatch::new()
                .level(log::LevelFilter::Info)
                .chain(fern::log_file("coreml.log").unwrap()),
        )
        .chain(
            fern::Dispatch::new()
                .level(log::LevelFilter::Warn)
                .chain(std::io::stdout()),
        )
        .apply()
        .unwrap();
}

fn print_int(i: i32) {
    print!("{i:}")
}
fn print_bool(i: i32) {
    if i == 0 {
        print!("false")
    } else if i == 1 {
        print!("true")
    } else {
        print!("non bool value {i:}")
    }
}

fn print_val() {
    print!("val")
}
fn print_colon() {
    print!(":")
}
fn print_arrow() {
    print!("->")
}
fn print_comma() {
    print!(",");
}
fn print_double_quote() {
    print!("\"")
}
fn print_lparen() {
    print!("(")
}
fn print_new_line() {
    print!("\n")
}
fn print_rparen() {
    print!(")")
}
fn print_equal() {
    print!("=")
}
fn print_space() {
    print!(" ")
}
/** Build Wasm Text to Wasm Binary
 * Called from SML#
 */
#[no_mangle]
extern "C" fn build_and_execute(src: *const c_char) {
    // safe SML# will pass Null terminated char sequence.
    std::io::stdout().flush().unwrap();
    info!("Wat to Wasm Builder and compiler started");
    let source = unsafe { CStr::from_ptr(src) };
    match source.to_str() {
        Ok(source) => {
            info!(
                "A webassembly text passed 
{}",
                source
            );

            let mut store = Store::default();
            match Module::new(&store, source) {
                Ok(module) => {
                    // install all functions. and memory.
                    let mut import_object = Imports::new();
                    let memory = match Memory::new(&mut store, MemoryType::new(2, None, false)) {
                        Ok(memory) => memory,
                        Err(err) => {
                            error!("{err:}");
                            return;
                        }
                    };
                    let string_view = memory.clone();
                    import_object.define("env", "linear_memory", memory.clone());
                    import_object.define(
                        "env",
                        "print_int",
                        Function::new_typed(&mut store, print_int),
                    );
                    import_object.define(
                        "env",
                        "print_bool",
                        Function::new_typed(&mut store, print_bool),
                    );
                    import_object.define(
                        "env",
                        "print_lparen",
                        Function::new_typed(&mut store, print_lparen),
                    );
                    import_object.define(
                        "env",
                        "print_rparen",
                        Function::new_typed(&mut store, print_rparen),
                    );
                    import_object.define(
                        "env",
                        "print_val",
                        Function::new_typed(&mut store, print_val),
                    );
                    import_object.define(
                        "env",
                        "print_comma",
                        Function::new_typed(&mut store, print_comma),
                    );
                    import_object.define(
                        "env",
                        "print_colon",
                        Function::new_typed(&mut store, print_colon),
                    );
                    import_object.define(
                        "env",
                        "print_equal",
                        Function::new_typed(&mut store, print_equal),
                    );
                    import_object.define(
                        "env",
                        "print_arrow",
                        Function::new_typed(&mut store, print_arrow),
                    );
                    import_object.define(
                        "env",
                        "print_space",
                        Function::new_typed(&mut store, print_space),
                    );
                    import_object.define(
                        "env",
                        "print_double_quote",
                        Function::new_typed(&mut store, print_double_quote),
                    );
                    import_object.define(
                        "env",
                        "print_new_line",
                        Function::new_typed(&mut store, print_new_line),
                    );
                    struct Env {
                        memory: Memory,
                    }
                    fn print_string(mut env: FunctionEnvMut<Env>, ptr_: i32, str_len_: i32) {
                        let (data, store) = env.data_and_store_mut();
                        if let Ok(memory_image) = data.memory.view(&store).copy_to_vec() {
                            let len = memory_image.len();
                            let ptr = ptr_ as usize;
                            let str_len = str_len_ as usize;
                            let end = ptr + str_len;
                            if end < len && ptr < len {
                                let str = &memory_image[ptr as usize..end as usize];
                                print!("{}", String::from_utf8_lossy(str));
                            } else {
                                log::error!("Invalid string passed to runtime! {ptr_:} {str_len_:}")
                            }
                        }
                    }
                    let env = FunctionEnv::new(
                        &mut store,
                        Env {
                            memory: string_view,
                        },
                    );
                    import_object.define(
                        "env",
                        "print_string",
                        Function::new_typed_with_env(&mut store, &env, print_string),
                    );

                    info!("generated imports");
                    match Instance::new(&mut store, &module, &import_object) {
                        Ok(instance) => {
                            if let Ok(main) = instance.exports.get_function("__cml_main") {
                                std::io::stdout().flush().unwrap();
                                info!("Execute finished");
                            } else {
                                info!("__cml_main not found");
                            }
                        }
                        Err(err) => {
                            warn!("{err:}");
                            return;
                        }
                    };
                }
                Err(error) => {
                    error!("Failed to compile Wasm {}", error);
                    return;
                }
            }
        }
        Err(error) => {
            error!("Compiler passed incorrect UTF-8 stream {}", error);
            return;
        }
    }
}
