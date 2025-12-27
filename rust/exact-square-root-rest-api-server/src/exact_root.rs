pub mod calc_exact_root {
  #[derive(Debug, PartialEq)]
  pub struct Res{
    pub multiplikator : i64,
    pub sqrt :i64
  }
// TODO: Return [Res]
// TODO: sqrt(2) -> []
  pub fn berechne_exacte_wurzel(radikand : i64) -> Vec<Res> {
    if radikand == 0 || radikand == 1 {
      vec![Res{multiplikator : -1, sqrt : radikand}]
    }else{
      let odd_numbers_1 = get_odd_numbers(radikand);
      let default_sqrts_1 = calc_default_sqrt(odd_numbers_1);
      let numbers_1 = get_numbers(radikand);
      let radicand_and_sqrt_1 = zip(default_sqrts_1, numbers_1);
      let simple_sqrt = search_simple_sqrt(radikand, radicand_and_sqrt_1);
      
      let odd_numbers_2 = get_odd_numbers(radikand);
      let default_sqrts_2 = calc_default_sqrt(odd_numbers_2);
      let numbers_2 = get_numbers(radikand);
      let radicand_and_sqrt_2 = zip(default_sqrts_2, numbers_2);
      // let complex_sqrt = search_complex_sqrt(radikand, radicand_and_sqrt_2.clone());
      let complex_sqrt_2 = search_complex_sqrt_2(radikand, radicand_and_sqrt_2);
      println!("{:?}", complex_sqrt_2);
      if !complex_sqrt_2.is_empty() {
        
        match simple_sqrt {
          Some(res) => {
            let mut result : Vec<Res> = vec![];
            for res_ in complex_sqrt_2{
              let sqrt = res_.1;
              result.push(Res{multiplikator : -1, sqrt : sqrt});
            }
            result
          },
          None => {
            let mut result : Vec<Res> = vec![];
            for res_ in complex_sqrt_2{
              result.push(Res{multiplikator : res_.0, sqrt : res_.1})
            }
            result
          }
        }
      }else{ 
        vec![Res{multiplikator : -1, sqrt : -1}]
      } 
    }
  }

  fn calc_default_sqrt(ungerade_zahlen: Vec<i64>) -> Vec<i64> {
    let mut counter = 0;
    let mut result : Vec<i64> = Vec::new();
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
  fn get_odd_numbers(radikand: i64) -> Vec<i64>{
    if radikand < 1 {
      vec![1]
    }else {
      (0..radikand).collect::<Vec<i64>>()
      .into_iter()
      .filter(|x| x % 2 != 0)
      .collect::<Vec<i64>>()
    }
    
  } 

  fn get_numbers(radikand: i64) -> Vec<i64> {
    let teiler = (radikand / 2) + 1;
    (2..teiler).collect::<Vec<i64>>()
  }

  fn zip(standard_werte: Vec<i64>, einfache_reihe: Vec<i64>) -> Vec<(i64,i64)> {
    let mut result : Vec<(i64, i64)> = Vec::new();
    for (x, y) in einfache_reihe.iter().zip(standard_werte.iter()) {
      result.push((*x, *y));
    }
    result
  }

  fn search_simple_sqrt(radikand: i64, radikand_wurzelwert: Vec<(i64, i64)>) -> Option<i64> {
    let result: Vec<(i64, i64)> = radikand_wurzelwert.into_iter()
        .filter(|(_, y)|  *y == radikand).collect::<Vec<(i64, i64)>>();
    if result.len() == 0 {
      None
    }else{
      Some(result[0].0)
    }
  }
      
  fn search_complex_sqrt(radikand: i64, radikand_wurzelwert: Vec<(i64, i64)>) -> Option<(i64, i64)> {
    let mut result: Option<(i64, i64)> = None;
    for (x, y) in radikand_wurzelwert.iter() {
      let temp = radikand % y;
      if temp == 0 {
        result = Some((*x, (radikand / y)));
        break;
      }
    } 
    result
  }

  fn search_complex_sqrt_2(radikand: i64, radikand_wurzelwert: Vec<(i64, i64)>) -> Vec<(i64, i64)> {
    let mut result: Vec<(i64, i64)> = vec![];
    for (x, y) in radikand_wurzelwert.iter() {
      let temp = radikand % y;
      if temp == 0 {
        result.push((*x, (radikand / y)));
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
        assert_eq!(berechne_exacte_wurzel(0), vec![Res{multiplikator : -1, sqrt : 0}]);
        // assert_eq!(berechne_exacte_wurzel(1), Res{multiplikator : -1, sqrt : 1});
        // assert_eq!(berechne_exacte_wurzel(2), Res{multiplikator : -1, sqrt : -1});
        // assert_eq!(berechne_exacte_wurzel(3), Res{multiplikator : -1, sqrt : -1});
        // assert_eq!(berechne_exacte_wurzel(4), Res{multiplikator : -1, sqrt : 2});
        // assert_eq!(berechne_exacte_wurzel(5), Res{multiplikator : -1, sqrt : -1});
        // assert_eq!(berechne_exacte_wurzel(6), Res{multiplikator : -1, sqrt : -1});
        // assert_eq!(berechne_exacte_wurzel(7), Res{multiplikator : -1, sqrt : -1});
        // assert_eq!(berechne_exacte_wurzel(8), Res{multiplikator : 2, sqrt : 2});
        // assert_eq!(berechne_exacte_wurzel(9), Res{multiplikator : -1, sqrt : 3});
        // assert_eq!(berechne_exacte_wurzel(10), Res{multiplikator : -1, sqrt : -1});
        // assert_eq!(berechne_exacte_wurzel(100), Res{multiplikator : -1, sqrt : 10});
        // assert_eq!(berechne_exacte_wurzel(1000), Res{multiplikator : 2, sqrt : 25});
    }
}