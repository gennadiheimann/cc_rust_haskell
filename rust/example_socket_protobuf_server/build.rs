fn main() {
    prost_build::compile_protos(&["proto/hello_world.proto"], &["proto/"]).unwrap();
}