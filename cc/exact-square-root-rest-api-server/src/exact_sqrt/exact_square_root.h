#ifndef EXACT_SQUARE_ROOT_H
#define EXACT_SQUARE_ROOT_H

#include <vector>
#include <optional>
#include <utility>

namespace exact_square_root {

struct Res{
  int multiplicator{0};
  int square_root{0};
};

class ExactSquareRoot {
  public:
  ExactSquareRoot(int radicand) : radicand_(radicand){};
  std::vector<Res> GetExactSqrt();

  // new imperative Implementation
  std::optional<int> SearchSimpleSqrt();
  std::vector<std::pair<int, int>> SearchComplexSqrt();
  
  
  private:
  void SetSqrtAndRadicand();
  // old impl
  std::vector<int> GetOddNumbers();
  std::vector<int> CalcDefaultSqrts(std::vector<int> odd_numbers);
  std::vector<std::pair<int, int>> GetRadicandAndSqrts() {return sqrt_and_radicand_;}
  void ZipRadicandsAndDefaultSqrts(std::vector<int> default_sqrts);
  
  
  int radicand_{0};
  std::vector<std::pair<int, int>> sqrt_and_radicand_;
};
}
#endif // EXACT_SQUARE_ROOT_H