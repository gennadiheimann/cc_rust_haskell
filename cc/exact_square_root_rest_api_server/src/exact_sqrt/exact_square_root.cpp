#include "exact_square_root.h"
#include <ranges>
#include <numeric>
#include <iostream>
#include <chrono>

namespace exact_square_root {
std::vector<Res> ExactSquareRoot::GetExactSqrt(){
  auto start = std::chrono::high_resolution_clock::now();
  if(radicand_  == 0 || radicand_ == 1){
    return {{-1, radicand_}};
  }
  const auto odd_numbers = GetOddNumbers();

  auto timepoint_1 = std::chrono::high_resolution_clock::now();
  auto duration_1 = std::chrono::duration_cast<std::chrono::milliseconds>(timepoint_1 - start).count();
  std::cout << "Timepoint 1: " << duration_1 << " Millisekunden\n";

  const auto default_sqrts = CalcDefaultSqrts(odd_numbers);

  auto timepoint_2 = std::chrono::high_resolution_clock::now();
  auto duration_2 = std::chrono::duration_cast<std::chrono::milliseconds>(timepoint_2 - timepoint_1).count();
  std::cout << "Timepoint 2: " << duration_2 << " Millisekunden\n";
  // TODO
  //std::vector<int>&&
  //std::vector<int>&
  ZipRadicandsAndDefaultSqrts(default_sqrts);

  auto timepoint_3 = std::chrono::high_resolution_clock::now();
  auto duration_3 = std::chrono::duration_cast<std::chrono::milliseconds>(timepoint_3 - timepoint_2).count();
  std::cout << "Timepoint 3: " << duration_3 << " Millisekunden\n";
  
  auto const simple_sqrt{SearchSimpleSqrt()}; 

  auto timepoint_4 = std::chrono::high_resolution_clock::now();
  auto duration_4 = std::chrono::duration_cast<std::chrono::milliseconds>(timepoint_4 - timepoint_3).count();
  std::cout << "Timepoint 4: " << duration_4 << " Millisekunden\n";

  if(simple_sqrt.has_value()){
    return {{-1, simple_sqrt.value()}};
  }
  auto const complex_sqrts{SearchComplexSqrt()};

  auto timepoint_5 = std::chrono::high_resolution_clock::now();
  auto duration_5 = std::chrono::duration_cast<std::chrono::milliseconds>(timepoint_5 - timepoint_4).count();
  std::cout << "Timepoint 4: " << duration_5 << " Millisekunden\n";

  auto duration_all = std::chrono::duration_cast<std::chrono::milliseconds>(timepoint_5 - start).count();
  std::cout << "Timepoint All: " << duration_all << " Millisekunden\n";

  if(complex_sqrts.empty()){
    return {};
  }else{
    std::vector<Res> r;
    for(const auto& complex_sqrt : complex_sqrts){
      r.push_back({complex_sqrt.first, complex_sqrt.second});
    }
    return r;
  }
  
}

std::vector<int> ExactSquareRoot::CalcDefaultSqrts(std::vector<int> odd_numbers){
  if(odd_numbers.size() < 2){
    return {};
  }

  // TODO: im rust auch aenliche Methode wie partial sum finden. 
  std::vector<int> result;
  result.reserve(odd_numbers.size());
  int acc = 0;
  for(int val : odd_numbers){
    if(acc < radicand_){
      acc += val;
      if(acc < 0){
        // std::cout << "#### acc: " << acc << std::endl;
        break;
      }
      result.push_back(acc);
    }else{
      break;
    }
  }

  result.erase(result.begin());
  return result;
}

std::vector<int> ExactSquareRoot::GetOddNumbers(){
  if(radicand_ < 1){
    return {};
  }
  std::vector<int> odds;
  int r {radicand_};
  if(radicand_ > 10000){
    r = radicand_* 0.01;
  }
  for (int x : std::views::iota(1, r) | std::views::filter([](int x){ return x & 1; })){
    odds.push_back(x);
  }
  return odds;
}

void ExactSquareRoot::ZipRadicandsAndDefaultSqrts(std::vector<int> default_sqrts){
  std::vector<int> radicands(default_sqrts.size());
  std::iota(radicands.begin(), radicands.end(), 2);
  for(int i = 0; i < radicands.size(); ++i){
    radicand_and_sqrt_.push_back({radicands.at(i), default_sqrts.at(i)});
  }
}

std::optional<int> ExactSquareRoot::SearchSimpleSqrt(){
  auto it = std::ranges::find_if(
    radicand_and_sqrt_, [this](const auto& r_and_s){return r_and_s.second == radicand_;});
  if(it != radicand_and_sqrt_.end()){
    return it->first;
  }else{
    return std::nullopt;
  }
}

std::vector<std::pair<int, int>> ExactSquareRoot::SearchComplexSqrt(){
  std::vector<std::pair<int, int>> result;
  for(const auto& r_and_s : radicand_and_sqrt_){
    int divisible = radicand_ % r_and_s.second;
    if(divisible == 0){
      result.push_back({r_and_s.first, (radicand_ / r_and_s.second)});
    }
  }
  return result;
}
}