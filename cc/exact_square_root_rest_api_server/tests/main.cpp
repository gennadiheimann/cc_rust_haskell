#include "catch2/catch_test_macros.hpp"
#include <cstdint>
#include "exact_square_root.h"
#include <limits>
#include <iostream>
#include <cmath>

TEST_CASE("sqrt(0)", ""){
  exact_square_root::ExactSquareRoot esr{0};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 1);
  REQUIRE(r.at(0).multiplicator == -1);
  REQUIRE(r.at(0).square_root == 0);
}

TEST_CASE("sqrt(1)", ""){
  exact_square_root::ExactSquareRoot esr{1};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 1);
  REQUIRE(r.at(0).multiplicator == -1);
  REQUIRE(r.at(0).square_root == 1);
}

TEST_CASE("sqrt(2)", ""){
  exact_square_root::ExactSquareRoot esr{2};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 0);
}

TEST_CASE("sqrt(3)", ""){
  exact_square_root::ExactSquareRoot esr{3};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 0);
}

TEST_CASE("sqrt(4)", ""){
  exact_square_root::ExactSquareRoot esr{4};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 1);
  REQUIRE(r.at(0).multiplicator == -1);
  REQUIRE(r.at(0).square_root == 2);
}

TEST_CASE("sqrt(5)", ""){
  exact_square_root::ExactSquareRoot esr{5};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 0);
}

TEST_CASE("sqrt(6)", ""){
  exact_square_root::ExactSquareRoot esr{6};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 0);
}

TEST_CASE("sqrt(7)", ""){
  exact_square_root::ExactSquareRoot esr{7};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 0);
}

TEST_CASE("sqrt(8)", ""){
  exact_square_root::ExactSquareRoot esr{8};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 1);
  REQUIRE(r.at(0).multiplicator == 2);
  REQUIRE(r.at(0).square_root == 2);
}

TEST_CASE("sqrt(9)", ""){
  exact_square_root::ExactSquareRoot esr{9};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 1);
  REQUIRE(r.at(0).multiplicator == -1);
  REQUIRE(r.at(0).square_root == 3);
}

TEST_CASE("sqrt(10)", ""){
  exact_square_root::ExactSquareRoot esr{10};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 0);
}

TEST_CASE("sqrt(11)", ""){
  exact_square_root::ExactSquareRoot esr{11};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 0);
}

TEST_CASE("sqrt(100)", ""){
  exact_square_root::ExactSquareRoot esr{100};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 1);
  REQUIRE(r.at(0).multiplicator == -1);
  REQUIRE(r.at(0).square_root == 10);
}

TEST_CASE("sqrt(1000)", ""){
  exact_square_root::ExactSquareRoot esr{1000};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  std::vector<std::pair<int, int>> expected {{2, 250}, {5, 40}, {10, 10}};
  REQUIRE(r.size() == 3);
  for(int i = 0; i < r.size(); ++i){
    REQUIRE(r.at(i).multiplicator == expected.at(i).first);
    REQUIRE(r.at(i).square_root == expected.at(i).second);
  }
}

TEST_CASE("Max calculble sqrt(92681)", "sqrt(max from type) * 2 - 1"){
  // std::cout << "Max calculble sqrt : " << (std::sqrt(std::numeric_limits<int>::max()) * 2) - 1 << "\n";
  exact_square_root::ExactSquareRoot esr{92681};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  REQUIRE(r.size() == 0);
}

TEST_CASE("sqrt(92680)", ""){
  exact_square_root::ExactSquareRoot esr{92680};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  std::vector<std::pair<int, int>> expected {{2, 23170}};  
  REQUIRE(r.size() == 1);
  REQUIRE(r.at(0).multiplicator == 2);
  REQUIRE(r.at(0).square_root == 23170);
}

TEST_CASE("sqrt(536870910)", ""){
  exact_square_root::ExactSquareRoot esr{536870900};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  std::vector<std::pair<int, int>> expected {{2, 134217725}, {5, 21474836}, {10, 5368709}};  
  REQUIRE(r.size() == 3);
  for(int i = 0; i < r.size(); ++i){
    REQUIRE(r.at(i).multiplicator == expected.at(i).first);
    REQUIRE(r.at(i).square_root == expected.at(i).second);
  }
}

TEST_CASE("sqrt(2147483647 )", "Max int to calc is 2147395600"){
  exact_square_root::ExactSquareRoot esr{2147395600};
  std::vector<exact_square_root::Res> r = esr.GetExactSqrt();
  std::vector<std::pair<int, int>> expected {{-1, 46340}};  
  REQUIRE(r.size() == 1);
  for(int i = 0; i < r.size(); ++i){
    REQUIRE(r.at(i).multiplicator == expected.at(i).first);
    REQUIRE(r.at(i).square_root == expected.at(i).second);
  }
}

// TEST_CASE ("GetOddNumbers", ""){
//   exact_square_root::ExactSquareRoot esr20{20};
//   std::vector<int> expected20 {1,3,5,7,9,11,13,15,17,19};
//   REQUIRE(esr20.GetOddNumbers() == expected20);

//   exact_square_root::ExactSquareRoot esr2{2};
//   std::vector<int> expected2 {1};
//   REQUIRE(esr2.GetOddNumbers() == expected2);

//   exact_square_root::ExactSquareRoot esr0{0};
//   std::vector<int> expected0 {};
//   REQUIRE(esr0.GetOddNumbers() == expected0);

//   exact_square_root::ExactSquareRoot esr1{1};
//   std::vector<int> expected1 {};
//   REQUIRE(esr1.GetOddNumbers() == expected1);

// }

// TEST_CASE ("CalcDefaultSqrt", ""){
//   exact_square_root::ExactSquareRoot esr20{20};
//   std::vector<int> odd_numbers20 = esr20.GetOddNumbers();
//   std::vector<int> expected20 {4, 9, 16, 25};
//   REQUIRE(esr20.CalcDefaultSqrts(odd_numbers20) == expected20);

//   exact_square_root::ExactSquareRoot esr49{49};
//   std::vector<int> odd_numbers49 = esr49.GetOddNumbers();
//   std::vector<int> expected49 {4, 9, 16, 25, 36, 49};
//   REQUIRE(esr49.CalcDefaultSqrts(odd_numbers49) == expected49);

//   exact_square_root::ExactSquareRoot esr0{0};
//   std::vector<int> odd_numbers0 = esr0.GetOddNumbers();
//   std::vector<int> expected0 {};
//   REQUIRE(esr0.CalcDefaultSqrts(odd_numbers0) == expected0);

//   exact_square_root::ExactSquareRoot esr1{1};
//   std::vector<int> odd_numbers1 = esr1.GetOddNumbers();
//   std::vector<int> expected1 {};
//   REQUIRE(esr1.CalcDefaultSqrts(odd_numbers1) == expected1);

//   exact_square_root::ExactSquareRoot esr2{2};
//   std::vector<int> odd_numbers2 = esr2.GetOddNumbers();
//   std::vector<int> expected2 {};
//   REQUIRE(esr2.CalcDefaultSqrts(odd_numbers2) == expected2);

//   exact_square_root::ExactSquareRoot esr3{3};
//   std::vector<int> odd_numbers3 = esr3.GetOddNumbers();
//   std::vector<int> expected3 {};
//   REQUIRE(esr3.CalcDefaultSqrts(odd_numbers3) == expected3);

//   exact_square_root::ExactSquareRoot esr4{4};
//   std::vector<int> odd_numbers4 = esr4.GetOddNumbers();
//   std::vector<int> expected4 {4};
//   REQUIRE(esr4.CalcDefaultSqrts(odd_numbers4) == expected4);
// }

// TEST_CASE ("ZipRadicandsAndDefaultSqrts", ""){
//   exact_square_root::ExactSquareRoot esr{20};
//   std::vector<int> odd_numbers = esr.GetOddNumbers();
//   std::vector<int> default_sqrts = esr.CalcDefaultSqrts(odd_numbers);
//   std::vector<std::pair<int, int>> expected {{2, 4}, {3, 9}, {4, 16}, {5, 25}, {6, 36}, {7, 49}, {8, 64}, {9, 81}, {10, 100}};
//   esr.ZipRadicandsAndDefaultSqrts(default_sqrts);
//   REQUIRE(esr.GetRadicandAndSqrts().size() == expected.size());
//   for(int i = 0; i < esr.GetRadicandAndSqrts().size(); ++i){
//     REQUIRE(esr.GetRadicandAndSqrts().at(i).first == expected.at(i).first);
//     REQUIRE(esr.GetRadicandAndSqrts().at(i).second == expected.at(i).second);
//   }
// }

// TEST_CASE ("SearchSimpleSqrt", ""){
//   exact_square_root::ExactSquareRoot esr4{4};
//   std::vector<int> odd_numbers4 = esr4.GetOddNumbers();
//   std::vector<int> default_sqrts4 = esr4.CalcDefaultSqrts(odd_numbers4);
//   esr4.ZipRadicandsAndDefaultSqrts(default_sqrts4);
//   std::optional<int> res4{esr4.SearchSimpleSqrt()};
//   REQUIRE(res4.value() == 2);

//   exact_square_root::ExactSquareRoot esr5{5};
//   std::vector<int> odd_numbers5 = esr5.GetOddNumbers();
//   std::vector<int> default_sqrts5 = esr5.CalcDefaultSqrts(odd_numbers5);
//   esr5.ZipRadicandsAndDefaultSqrts(default_sqrts5);
//   std::optional<int> res5{esr5.SearchSimpleSqrt()};
//   REQUIRE_FALSE(res5.has_value());

//   exact_square_root::ExactSquareRoot esr0{0};
//   std::vector<int> odd_numbers0 = esr0.GetOddNumbers();
//   std::vector<int> default_sqrts0 = esr0.CalcDefaultSqrts(odd_numbers0);
//   esr0.ZipRadicandsAndDefaultSqrts(default_sqrts0);
//   std::optional<int> res0{esr0.SearchSimpleSqrt()};
//   REQUIRE_FALSE(res0.has_value());

//   exact_square_root::ExactSquareRoot esr1{1};
//   std::vector<int> odd_numbers1 = esr1.GetOddNumbers();
//   std::vector<int> default_sqrts1 = esr1.CalcDefaultSqrts(odd_numbers1);
//   esr1.ZipRadicandsAndDefaultSqrts(default_sqrts1);
//   std::optional<int> res1{esr1.SearchSimpleSqrt()};
//   REQUIRE_FALSE(res1.has_value());

// }

// TEST_CASE ("SearchComplexSqrt", ""){
//   exact_square_root::ExactSquareRoot esr8{8};
//   std::vector<int> odd_numbers8= esr8.GetOddNumbers();
//   std::vector<int> default_sqrts8 = esr8.CalcDefaultSqrts(odd_numbers8);
//   esr8.ZipRadicandsAndDefaultSqrts(default_sqrts8);
//   std::vector<std::pair<int, int>> res8{esr8.SearchComplexSqrt()};
//   std::vector<std::pair<int, int>> expected {{2, 2}};
//   REQUIRE(res8 == expected);
//   REQUIRE(res8.size() == 1);
//   for(int i = 0; i < res8.size(); ++i){
//     REQUIRE(res8.at(i).first == expected.at(i).first);
//     REQUIRE(res8.at(i).second == expected.at(i).second);
//   }

//   exact_square_root::ExactSquareRoot esr10{10};
//   std::vector<int> odd_numbers10 = esr10.GetOddNumbers();
//   std::vector<int> default_sqrts10 = esr10.CalcDefaultSqrts(odd_numbers10);
//   esr10.ZipRadicandsAndDefaultSqrts(default_sqrts10);
//   std::vector<std::pair<int, int>> res10{esr10.SearchComplexSqrt()};
//   std::vector<std::pair<int, int>> expected10 {};
//   REQUIRE(res10 == expected10);
//   REQUIRE(res10.size() == 0);

//   exact_square_root::ExactSquareRoot esr1000{1000};
//   std::vector<int> odd_numbers1000 = esr1000.GetOddNumbers();
//   std::vector<int> default_sqrts1000 = esr1000.CalcDefaultSqrts(odd_numbers1000);
//   esr1000.ZipRadicandsAndDefaultSqrts(default_sqrts1000);
//   std::vector<std::pair<int, int>> res1000{esr1000.SearchComplexSqrt()};
//   std::vector<std::pair<int, int>> expected1000 {{2, 250}, {5, 40}, {10, 10}};
//   REQUIRE(res1000 == expected1000);
//   REQUIRE(res1000.size() == 3);
//   for(int i = 0; i < res1000.size(); ++i){
//     REQUIRE(res1000.at(i).first == expected1000.at(i).first);
//     REQUIRE(res1000.at(i).second == expected1000.at(i).second);
//   }
// }

