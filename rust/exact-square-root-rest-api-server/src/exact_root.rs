pub mod calc_exact_root {
  #[derive(Debug, PartialEq)]
  pub struct Res{
    pub multiplikator : u64,
    pub wurzelwert :u64
  }

  pub fn berechne_exacte_wurzel(radikand : u64) -> Res {
    let ungerade_zahlen_1 = berechne_ungerade_zahlen(radikand);
    let standard_werte_1 = berechne_standard_werte(ungerade_zahlen_1);
    let einfache_reihe_1 = berechne_einfache_reihe(radikand);
    let radikand_wurzelwert_1 = zippen(standard_werte_1, einfache_reihe_1);
    let einfache_wurzelwert = berechne_einfache_wurzelwert(radikand, radikand_wurzelwert_1);
    
    let ungerade_zahlen_2 = berechne_ungerade_zahlen(radikand);
    let standard_werte_2 = berechne_standard_werte(ungerade_zahlen_2);
    let einfache_reihe_2 = berechne_einfache_reihe(radikand);
    let radikand_wurzelwert_2 = zippen(standard_werte_2, einfache_reihe_2);
    let komplexe_wurzelwert = berechne_komplexe_wurzelwert(radikand, radikand_wurzelwert_2);
    let multiplikator = komplexe_wurzelwert.unwrap().0;
    let wurzelwert = komplexe_wurzelwert.unwrap().1;
    match einfache_wurzelwert {
      Some(res) => Res{multiplikator : 1, wurzelwert : res},
      None => Res{multiplikator : multiplikator, wurzelwert : wurzelwert}
    }
    // unberehenbare Wurzel loesen panic aus
  }
  //die Berechnung bis Resultat der Addition < Radikand
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
    if radikand < 1 {
      vec![1]
    }else {
      (0..radikand).collect::<Vec<u64>>()
      .into_iter()
      .filter(|x| x % 2 != 0)
      .collect::<Vec<u64>>()
    }
    
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
    for (x, y) in radikand_wurzelwert.iter() {
      let temp = radikand % y;
      if temp == 0 {
        result = Some((*x, (radikand / y)));
        break;
      }
    } 
    result
  }
}

#[cfg(test)]
mod tests {
    use crate::exact_root::calc_exact_root::berechne_exacte_wurzel;
    use crate::exact_root::calc_exact_root::Res;

    #[test]
    fn test_get_exact_sqrt() {
        assert_eq!(berechne_exacte_wurzel(0), Res{multiplikator : 1, wurzelwert : 0});
        // assert_eq!(berechne_exacte_wurzel(1), Res{multiplikator : 1, wurzelwert : 1});
        // assert_eq!(berechne_exacte_wurzel(2), Res{multiplikator : 1, wurzelwert : 2});
        // assert_eq!(berechne_exacte_wurzel(3), Res{multiplikator : 1, wurzelwert : 3});
        // assert_eq!(berechne_exacte_wurzel(4), Res{multiplikator : 1, wurzelwert : 2});
        // assert_eq!(berechne_exacte_wurzel(5), Res{multiplikator : 1, wurzelwert : 2});
        // assert_eq!(berechne_exacte_wurzel(6), Res{multiplikator : 1, wurzelwert : 2});
        // assert_eq!(berechne_exacte_wurzel(7), Res{multiplikator : 1, wurzelwert : 2});
        // assert_eq!(berechne_exacte_wurzel(8), Res{multiplikator : 1, wurzelwert : 2});
        // assert_eq!(berechne_exacte_wurzel(9), Res{multiplikator : 1, wurzelwert : 2});
        // assert_eq!(berechne_exacte_wurzel(10), Res{multiplikator : 1, wurzelwert : 2});
        // assert_eq!(berechne_exacte_wurzel(100), Res{multiplikator : 1, wurzelwert : 2});
        // assert_eq!(berechne_exacte_wurzel(1000), Res{multiplikator : 1, wurzelwert : 2});
    }
}