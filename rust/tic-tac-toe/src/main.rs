#[derive(Copy, Clone, Debug, PartialEq)]
enum Player{
  X,
  O,
  N
}

/*
GameState {
  New, nextX, nextO, XWon, OWon
}

AreaNum {
  num [1..9]
}

1. Clent Request
{
  "is_new_game" : true/false
  "is_game_won" : XWon/OWon
  "area_num" : ""
}

2. Server Respons
{
  "game_state" : ""
}

*/
use std::io::{self, Write};
fn main() {
  println!("TIC TAC TOY");
  println!("Abrechen mit C");
  println!("Startet immer der Player X");
  println!("Gib den Feldnummer");
  println!("==================================");
  let mut area: [Player; 9] = [Player::N; 9];
  let win_pattern: [(i8, i8, i8); 8] = [(1, 4, 7), 
                                        (2, 5, 8),
                                        (3, 6, 9),
                                        (1, 2, 3),
                                        (4, 5, 6),
                                        (7, 8, 9),
                                        (1, 5, 9),
                                        (7, 5, 3)];
                                        // let win_pattern_1 :[[Player; 9]; 8] =[[Player::X]]
  let mut next_player = Player::X;
  loop{
    let mut input = String::new();
    print!("Feld: ");
    io::stdout().flush().unwrap();
    io::stdin().read_line(&mut input).expect("Error");
    let input_trim = input.trim();
    if input_trim == "C" {
      break;
    }
    match input.trim().parse::<i8>(){
      Ok(area_num) => {
        if !(area_num > 0 && area_num < 10) {
          println!("Die Zahl soll zwischen 1 und 9 sein");
        }else{
          if next_player == Player::X {
            let is_area_not_touched = area[(area_num - 1) as usize] == Player::N;
            if is_area_not_touched {
              println!("Player X : {}", area_num);
              area[(area_num - 1) as usize] = Player::X;
              for n in &area{
                print!(" {:?}", n);
              }
              println!("");
              if has_x_won(&area, &win_pattern){
                println!("X WON!!!");
                break;
              }
              println!("Next Player O");
              next_player = Player::O;
            }
            else{
              println!("Diser Feld ist schon belegt")
            }
          }else{
            let is_area_not_touched = area[(area_num - 1) as usize] == Player::N;
            if is_area_not_touched {
              println!("Player O : {}", area_num);
              area[(area_num - 1) as usize] = Player::O;
              for n in &area{
                print!(" {:?}", n);
              }
              println!("");
              if has_o_won(&area, &win_pattern) {
                println!("O WON!!!");
                break;
              }
              println!("Next Player X");
              next_player = Player::X;
            }else{
              println!("Diser Feld ist schon belegt")
            }
          }
        }
      },
      Err(_) => {println!("Ist kein zahl");}
    }
  }
}

fn has_x_won(area: &[Player; 9], win_pattern: &[(i8, i8, i8); 8]) -> bool {
  win_pattern.iter().find(|(x, y, z)| {
    area[(*x - 1) as usize] == Player::X &&
    area[(*y - 1) as usize] == Player::X &&
    area[(*z - 1) as usize] == Player::X
  }).is_some()
}

fn has_o_won(area: &[Player; 9], win_pattern: &[(i8, i8, i8); 8]) -> bool {
  win_pattern.iter().find(|(x, y, z)| {
    area[(*x - 1) as usize] == Player::O &&
    area[(*y - 1) as usize] == Player::O &&
    area[(*z - 1) as usize] == Player::O
  }).is_some()
}     
