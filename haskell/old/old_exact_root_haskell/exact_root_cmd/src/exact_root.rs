/*------------------------------------------------------------------------------

  Exacte Berechnung der Wurzel
  Die Berechnung beruht auf das Prenzip der Summe ungeraden Reihen.
  
  Beispiel sqrt(50)
  1 + 3 = [4]
  4 + 5 = [4,9]
  9 + 7 = [4,9,16]
  16 + 9 = [4,9,16,25]
  25 + 11 = [4,9,16,25,36]
  36 + 13 = [4,9,16,25,36,49]
  
  1. Berechne ungerade Reihe bis 50
  2. Berechne in der ungerade Reihe die summe der ersten zwei Elemente und hänge
     diese an der neue Reihe (siehe oben). 
  3. Parallel zu der o.g. Reihe wird zu jedem Wert einen Wert aus der einfache 
     Reihe angehaengt z.B. [(2,4),(3,9),(4,16)]. In weiteren Beschreibung wird 
     diese Reihe als Wurzelwert_Radikand genannt.
     Funktion : berechneStandardwerte Int -> StandardWerte
  4. Berechne einfache Wurzelwert z.B. sqrt(25) = 5
     Dazu wird einfach in der Reihe Wurzelwert_Radikand der zweite Wert 
     (Radikand) verglichen und der erste Wert (Wurzelwert) als Ergebnis
     ausgegeben
     Funktion : berechneEinfacheWurzelwert
  5. Berechnung des komplexen Wurzelwertes z.B. sqrt(50) = 5 * sqrt(2)
     Bei der komplexen Berechnung wird iterativ versucht jeden Radikand aus der 
     Reihe Radikand_Wurzelwert mit Hilfe der quotRem funktion zu teilen.
     Wenn einen Teiler ohne Rest gefunden wird wird der Ergebnis zu dem
     unberechnetem Radikand unter Wurzel und der Wurzelwert aus der Reihe 
     Werzelwert_Radikand zu dem Multiplikator. 
     5 = Multiplikator
     sqrt(2) =  unberechneter Radikand
  ------------------------------------------------------------------------------*/


pub fn berechne_exacte_wurzel(radikand : u64) -> String {
  let ungerade_zahlen_1 = berechne_ungerade_zahlen(radikand);
  let standard_werte_1 = berechne_standard_werte(ungerade_zahlen_1);
  let einfache_reihe_1 = berechne_einfache_reihe(radikand);
  let radikand_wurzelwert_1 = zippen(standard_werte_1, einfache_reihe_1);
  let einfache_wurzelwert = berechne_einfache_wurzelwert(radikand, radikand_wurzelwert_1);
  //println!("Einfache Wurzelwert {:?}", einfache_wurzelwert);
  let ungerade_zahlen_2 = berechne_ungerade_zahlen(radikand);
  let standard_werte_2 = berechne_standard_werte(ungerade_zahlen_2);
  let einfache_reihe_2 = berechne_einfache_reihe(radikand);
  let radikand_wurzelwert_2 = zippen(standard_werte_2, einfache_reihe_2);
  let komplexe_wurzelwert = berechne_komplexe_wurzelwert(radikand, radikand_wurzelwert_2);
  //println!("Komplexe Wurzelwert {:?}", komplexe_wurzelwert);
  match (einfache_wurzelwert, komplexe_wurzelwert) {
    (Some(einfach), Some(_)) => einfach.to_string(),
    (Some(_), None)          => String::from("unbehandelter Fall"),
    (None, Some(komplex))          => {
                                        let multiplikator = komplex.0.to_string();
                                        let wurzelwert = komplex.1.to_string();
                                        multiplikator + "*sqrt(" + &wurzelwert + ")"
                                      },
    (None, None)                   => {
                                        String::from("unberehenbar")
                                      }
  }
}

fn berechne_standard_werte(ungerade_zahlen: Vec<u64>) -> Vec<u64> {
  let mut counter = 0;
  let mut result : Vec<u64> = Vec::new();
  let size = ungerade_zahlen.len();
  while counter < (size - 1) {
    if counter == 0 {
      let temp = ungerade_zahlen[counter] + ungerade_zahlen[counter + 1];
      result.push(temp);
    }else{
      let temp = result[counter - 1] + ungerade_zahlen[counter + 1];
      result.push(temp);
    }
    counter += 1;
  }
  result
}

use std::vec::Vec;
fn berechne_ungerade_zahlen(radikand: u64) -> Vec<u64>{
  (0..radikand).collect::<Vec<u64>>()
    .into_iter()
    .filter(|x| x % 2 != 0)
    .collect::<Vec<u64>>()
} 

fn berechne_einfache_reihe(radikand: u64) -> Vec<u64> {
  let teiler = (radikand / 2) + 1;
  (2..teiler).collect::<Vec<u64>>()
}

fn zippen(standard_werte: Vec<u64>, einfache_reihe: Vec<u64>) -> Vec<(u64,u64)> {
  let mut result : Vec<(u64, u64)> = Vec::new();
  for (x, y) in einfache_reihe.iter().zip(standard_werte.iter()) {
    result.push((*x, *y));
  }
  result
}

fn berechne_einfache_wurzelwert(radikand: u64, radikand_wurzelwert: Vec<(u64, u64)>) -> Option<u64> {
  let result: Vec<(u64, u64)> = radikand_wurzelwert.into_iter()
      .filter(|(_, y)|  *y == radikand).collect::<Vec<(u64, u64)>>();
  if result.len() == 0 {
    None
  }else{
    Some(result[0].0)
  }
}
    
fn berechne_komplexe_wurzelwert(radikand: u64, radikand_wurzelwert: Vec<(u64, u64)>) -> Option<(u64, u64)> {
  let mut result: Option<(u64, u64)> = None;
  // println!("radikand_wurzelwert {:?}", radikand_wurzelwert);
  for (x, y) in radikand_wurzelwert.iter() {
    let temp = radikand % y;
    // println!("temp = radikand % y; {} {} {}", temp, radikand, y);
    if temp == 0 {
      result = Some((*x, (radikand / y)));
      break;
    }
  } 
  result
}