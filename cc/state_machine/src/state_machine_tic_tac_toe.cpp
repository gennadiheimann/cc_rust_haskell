#include <iostream>
#include <ranges>

#include "state_machine_tic_tac_toe.h"

namespace {

  const std::array<std::array<int, 3>, 8> kWinPattern{{
  {0, 3, 6}, {1, 4, 7}, {2, 5, 8}, {0, 1, 2}, {3, 4, 5}, {6, 7, 8}, {0, 4, 8}, {6, 4, 2}}};

  auto GetUserInput(const std::string& player) -> int {
  int num{};
  std::string input{};
  while(true){
    std::cout << "Geben Sie Feldnummer für " << player << ": ";
    std::getline(std::cin, input);
    try {
      num = std::stoi(input);
      if(num > 0 && num < 10){
        break;
      }
      std::cout << "Der Feldnummer soll zwischen 1 und 9 sein?" << std::endl;
    } catch (...) {
      std::cout << "Es sind nur Zahlen erlaubt!" << std::endl;
    }
  }
  return num - 1;
}
}

namespace state_machine_tic_tac_toe {

auto InitGame::PrintInitGame() -> void {
  std::cout << "Start Game" << std::endl;
}

auto PlayerX::GetAreaNumber() -> int {
  return GetUserInput("Player X");
}

auto PlayerO::GetAreNumber() -> int {
  return GetUserInput("Player O");
}

StateMachineTicTacToe::StateMachineTicTacToe() : state_(InitGame()) {}

auto StateMachineTicTacToe::StartGame() -> void {
  std::get<InitGame>(state_).PrintInitGame();
  area_.fill(Player::N);
  NextPlayerX();
}

auto StateMachineTicTacToe::NextPlayerX()-> void {
  state_ = PlayerX();
  auto area_num = std::get<PlayerX>(state_).GetAreaNumber();
  std::cout << "Player X Engabe : " << area_num << std::endl;
  AddFeldNumber(area_num);
  PrintArea();
  if(HasPlayerWon()){
    FinishGame();
  }else{
    NextPlayerO();
  }
}

auto StateMachineTicTacToe::NextPlayerO() -> void {
  state_ = PlayerO();
  auto area_num = std::get<PlayerO>(state_).GetAreNumber();
  std::cout << "Player O Engabe : " << area_num << std::endl;
  AddFeldNumber(area_num);
  PrintArea();
  if(HasPlayerWon()){
    FinishGame();
  }else{
    NextPlayerX();
  }
}

auto StateMachineTicTacToe::FinishGame() -> void {
  if(std::holds_alternative<PlayerX>(state_)){
    std::cout << "Player X hat gewonnen" << std::endl;
  }
  if(std::holds_alternative<PlayerO>(state_)) {
    std::cout << "Player O hat gewonnen" << std::endl;
  }
  exit(0);
}

auto StateMachineTicTacToe::AddFeldNumber(int num) -> void {
    if(std::holds_alternative<PlayerX>(state_)){
      if(area_.at(num) == Player::N){
        area_.at(num) = Player::X;
      }else{
        std::cout << "Der Feldnummer ist schon belegt!!!" << std::endl;
        NextPlayerX();
      }
  }
  if(std::holds_alternative<PlayerO>(state_)) {
    if(area_.at(num) == Player::N){
      area_.at(num) = Player::O;
    }else{
      std::cout << "Der Feldnummer ist schon belegt!!!" << std::endl;
      NextPlayerO();
    }
  }
}

auto StateMachineTicTacToe::HasPlayerWon() -> bool {
  if(std::holds_alternative<PlayerX>(state_)){
    auto winning_patterns = 
      kWinPattern | std::views::filter([&](const auto& p){
        return  area_.at(p.at(0)) == Player::X &&
                area_.at(p.at(1)) == Player::X &&
                area_.at(p.at(2)) == Player::X;
    });
    if (!winning_patterns.empty()) {
        return true;
    }
    return false;
  }

  if(std::holds_alternative<PlayerO>(state_)) {
    auto winning_patterns = 
      kWinPattern | std::views::filter([&](const auto& p){
        return  area_.at(p.at(0)) == Player::O &&
                area_.at(p.at(1)) == Player::O &&
                area_.at(p.at(2)) == Player::O;
    });
    if (!winning_patterns.empty()) {
        return true;
    }
    return false;
  }
  throw std::runtime_error("Fehler in Trasition zwischen States");
}

auto StateMachineTicTacToe::PlayerToString(Player player) -> std::string {
  switch (player) {
    case Player::N: return "N";
    case Player::X: return "X";
    case Player::O: return "O";
  }
  return "";
}

auto StateMachineTicTacToe::PrintArea() -> void {
  int count{1};
  for(const auto n : area_){
    std::cout << PlayerToString(n) << " ";
    if(count % 3 == 0){
      std::cout << std::endl;
    }
    count++;
  }
}
}