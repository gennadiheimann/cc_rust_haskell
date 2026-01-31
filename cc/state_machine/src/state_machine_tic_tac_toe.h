#ifndef SRC_STATE_MACHINE_TIC_TAC_TOE_H
#define SRC_STATE_MACHINE_TIC_TAC_TOE_H

#include <iostream>
#include <variant>
#include <array>
#include <ranges>

namespace state_machine_tic_tac_toe {
/**
 * State PlayerX, PlayerO
 * Event StartGame, NextPlayer, EndGame
 */

const std::array<std::array<int, 3>, 8> kWinPattern{{
  {0, 3, 7}, {1, 4, 7}, {2, 5, 8}, {0, 1, 2}, {3, 4, 5}, {6, 7, 8}, {0, 4, 8}, {6, 4, 2}}};

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

class InitGame {
public:
  auto PrintInitGame() {
    std::cout << "Start Game" << std::endl;
  } 
};

class PlayerX {
public:
  PlayerX(){
    // std::cout << "FeldNummer für Player X: ";
  }

  auto GetAreaNumber() -> int {
    return GetUserInput("Player X");
  }
};

class PlayerO {
public:
  PlayerO(){
    // std::cout << "FeldNummer für Player O: ";
  }

  auto GetAreNumber() -> int {
    return GetUserInput("Player O");
  }
};

class StateMachineTicTacToe{
public:
  StateMachineTicTacToe() : state_(InitGame()) {}
  auto StartGame() -> void {
    std::get<InitGame>(state_).PrintInitGame();
    area_.fill(Player::N);
    NextPlayerX();
  }

  auto NextPlayerX()-> void {
    state_ = PlayerX();
    auto area_num = std::get<PlayerX>(state_).GetAreaNumber();
    std::cout << "Player X Engabe : " << area_num << std::endl;
    AddFeldNumber(area_num);
    for(const auto n : area_){
      std::cout << PlayerToString(n) << " ";
    }
    std::cout << std::endl;
    if(HasPlayerWon()){
      FinishGame();
    }else{
      NextPlayerO();
    }
  }

  auto NextPlayerO() -> void {
    state_ = PlayerO();
    auto area_num = std::get<PlayerO>(state_).GetAreNumber();
    std::cout << "Player O Engabe : " << area_num << std::endl;
    AddFeldNumber(area_num);
    for(const auto n : area_){
      std::cout << PlayerToString(n) << " ";
    }
    std::cout << std::endl;
    if(HasPlayerWon()){
      FinishGame();
    }else{
      NextPlayerX();
    }
  }

  auto FinishGame() -> void {
    if(std::holds_alternative<PlayerX>(state_)){
      std::cout << "Player X hat gewonnen" << std::endl;
    }
    if(std::holds_alternative<PlayerO>(state_)) {
      std::cout << "Player O hat gewonnen" << std::endl;
    }
    exit(0);
  }

  auto AddFeldNumber(int num) -> void {
     if(std::holds_alternative<PlayerX>(state_)){
      area_[num] = Player::X;
    }
    if(std::holds_alternative<PlayerO>(state_)) {
      area_[num] = Player::O;
    }
  }

  auto HasPlayerWon() -> bool {
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

  private:
    std::variant<InitGame, PlayerX, PlayerO> state_{};
    enum class Player{X, O, N};
    std::array<Player, 9> area_{};
    std::string PlayerToString(Player player){
      switch (player) {
        case Player::N: return "N";
        case Player::X: return "X";
        case Player::O: return "O";
      }
      return "";
    }
};
}
#endif // SRC_STATE_MACHINE_TIC_TAC_TOE_H