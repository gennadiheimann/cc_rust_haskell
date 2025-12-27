pub mod exact_root {
  #[derive(Debug, PartialEq)]
  pub struct Res{
    pub multiplicator : i64,
    pub sqrt :i64
  }

  pub fn get_exact_sqrt(radicand : i64) -> Vec<Res> {
    if radicand == 0 || radicand == 1 {
      vec![Res{multiplicator : -1, sqrt : radicand}]
    }else{
      let odd_numbers_1 = get_odd_numbers(radicand);
      let default_sqrts_1 = calc_default_sqrt(odd_numbers_1);
      let numbers_1 = get_numbers(radicand);
      let radicand_and_sqrt_1 = zip(default_sqrts_1, numbers_1);
      let simple_sqrt = search_simple_sqrt(radicand, radicand_and_sqrt_1);
      
      let odd_numbers_2 = get_odd_numbers(radicand);
      let default_sqrts_2 = calc_default_sqrt(odd_numbers_2);
      let numbers_2 = get_numbers(radicand);
      let radicand_and_sqrt_2 = zip(default_sqrts_2, numbers_2);
      let complex_sqrt = search_complex_sqrt(radicand, radicand_and_sqrt_2);
      match simple_sqrt {
        Some(res) => {
            vec![Res{multiplicator : -1, sqrt : res}]
        },
        None => {
          if !complex_sqrt.is_empty() {
            let mut result : Vec<Res> = vec![];
            for res_ in complex_sqrt{
              result.push(Res{multiplicator : res_.0, sqrt : res_.1})
            }
            result
          }else{ 
            vec![Res{multiplicator : -1, sqrt : -1}]
          } 
        }
      }
    }
  }

  fn calc_default_sqrt(odd_numbers: Vec<i64>) -> Vec<i64> {
    let mut counter = 0;
    let mut result : Vec<i64> = Vec::new();
    let size = odd_numbers.len();
    while counter < (size - 1) {
      if counter == 0 {
        let temp = odd_numbers[counter] + odd_numbers[counter + 1];
        result.push(temp);
      }else{
        let temp = result[counter - 1] + odd_numbers[counter + 1];
        result.push(temp);
      }
      counter += 1;
    }
    result
  }

  use std::vec::Vec;
  fn get_odd_numbers(radicand: i64) -> Vec<i64>{
    if radicand < 1 {
      vec![1]
    }else {
      (0..radicand).collect::<Vec<i64>>()
      .into_iter()
      .filter(|x| x % 2 != 0)
      .collect::<Vec<i64>>()
    }
    
  } 

  fn get_numbers(radicand: i64) -> Vec<i64> {
    let devider = (radicand / 2) + 1;
    (2..devider).collect::<Vec<i64>>()
  }

  fn zip(default_sqrts: Vec<i64>, numbers: Vec<i64>) -> Vec<(i64,i64)> {
    let mut result : Vec<(i64, i64)> = Vec::new();
    for (x, y) in numbers.iter().zip(default_sqrts.iter()) {
      result.push((*x, *y));
    }
    result
  }

  fn search_simple_sqrt(radicand: i64, radicand_and_sqrt: Vec<(i64, i64)>) -> Option<i64> {
    let result: Vec<(i64, i64)> = radicand_and_sqrt.into_iter()
        .filter(|(_, y)|  *y == radicand).collect::<Vec<(i64, i64)>>();
    if result.len() == 0 {
      None
    }else{
      Some(result[0].0)
    }
  }

  fn search_complex_sqrt(radicand: i64, radicand_and_sqrt: Vec<(i64, i64)>) -> Vec<(i64, i64)> {
    let mut result: Vec<(i64, i64)> = vec![];
    for (x, y) in radicand_and_sqrt.iter() {
      let temp = radicand % y;
      if temp == 0 {
        result.push((*x, (radicand / y)));
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
        assert_eq!(berechne_exacte_wurzel(0), vec![Res{multiplicator : -1, sqrt : 0}]);
        assert_eq!(berechne_exacte_wurzel(1), vec![Res{multiplicator : -1, sqrt : 1}]);
        assert_eq!(berechne_exacte_wurzel(2), vec![Res{multiplicator : -1, sqrt : -1}]);
        assert_eq!(berechne_exacte_wurzel(3), vec![Res{multiplicator : -1, sqrt : -1}]);
        assert_eq!(berechne_exacte_wurzel(4), vec![Res{multiplicator : -1, sqrt : 2}]);
        assert_eq!(berechne_exacte_wurzel(5), vec![Res{multiplicator : -1, sqrt : -1}]);
        assert_eq!(berechne_exacte_wurzel(6), vec![Res{multiplicator : -1, sqrt : -1}]);
        assert_eq!(berechne_exacte_wurzel(7), vec![Res{multiplicator : -1, sqrt : -1}]);
        assert_eq!(berechne_exacte_wurzel(8), vec![Res{multiplicator : 2, sqrt : 2}]);
        assert_eq!(berechne_exacte_wurzel(9), vec![Res{multiplicator : -1, sqrt : 3}]);
        assert_eq!(berechne_exacte_wurzel(10), vec![Res{multiplicator : -1, sqrt : -1}]);
        assert_eq!(berechne_exacte_wurzel(100), vec![Res{multiplicator : -1, sqrt : 10}]);
        assert_eq!(berechne_exacte_wurzel(1000), vec![Res { multiplicator: 2, sqrt: 250 }, 
                                                      Res { multiplicator: 5, sqrt: 40 }, 
                                                      Res { multiplicator: 10, sqrt: 10 }]);
        // assert_eq!(berechne_exacte_wurzel(123956844), vec![Res{multiplicator : 2, sqrt : 30989211}]);
    }
}