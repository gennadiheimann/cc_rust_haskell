use std::env;

mod exact_root;

fn main() {
  let args: Vec<String> = env::args().collect();

  let result: String = match &args[1].parse() {
    Ok(rad) => exact_root::berechne_exacte_wurzel(*rad),
    Err(e) => {
      println!("Error : {}", e); 
      String::new()
    }
  };

  println!("Exakte Wurzel ist {}", result);
}
