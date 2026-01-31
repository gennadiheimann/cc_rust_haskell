#ifndef SRC_STATE_MACHINE_TIC_TAC_TOE_H
#define SRC_STATE_MACHINE_TIC_TAC_TOE_H

#include <iostream>
#include <variant>
#include <array>

namespace state_machine_tic_tac_toe {
/**
 * State PlayerX, PlayerO
 * Event StartGame, NextPlayer, EndGame
 */

const std::array<std::array<int, 3>, 8> win_pattern{{
  {1, 4, 7}, {2, 5, 8}, {3, 6, 9}, {1, 2, 3}, {4, 5, 5}, {7, 8, 9}, {1, 5, 9}, {7, 5, 3}}};

class InitGame {
public:
  auto PrintInitGame() {
    std::cout << "Start Game" << std::endl;
  } 
};

class PlayerX {
public:
  PlayerX(){
    std::cout << "FeldNummer für Player X: ";
  }

  auto GetAreNumber() -> int {
    int num{};
    std::string input{};
    while(true){
      std::cout << "FeldNummer für Player X: ";
      std::getline(std::cin, input);
      try {
        int num = std::stoi(input);
        if(num > 0 && num < 10){
          std::cout << "Zahl: " << num << "\n";
          break;
        }
      } catch (...) {
        std::cout << "Keine Zahl!\n";
      }
    }
    return num;
  }
};

class PlayerO {
public:
  PlayerO(){
    std::cout << "FeldNummer für Player O: ";
  }

  auto GetAreNumber() -> int {
    int num{};
    std::cin >> num;
    return num;
  }
};

class StateMachineTicTacToe{
public:
  StateMachineTicTacToe() : state_(InitGame()) {}
  auto StartGame() -> void {
    std::get<InitGame>(state_).PrintInitGame();
    NextPlayerX();
  }

  auto NextPlayerX()-> void {
    state_ = PlayerX();
    auto area_num = std::get<PlayerX>(state_).GetAreNumber();
    std::cout << "Player X Number : " << area_num << std::endl;
    if(area_num == 5){
      FinishGame();
    }else{
      NextPlayerO();
    }
  }

  auto NextPlayerO() -> void {
    state_ = PlayerO();
    auto area_num = std::get<PlayerO>(state_).GetAreNumber();
    std::cout << "Player O Number : " << area_num << std::endl;
    if(area_num == 7){
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

  private: 
    std::variant<InitGame, PlayerX, PlayerO> state_{};
    enum class Player{X, O, N};
    std::array<Player, 9> area_;
};
}
#endif // SRC_STATE_MACHINE_TIC_TAC_TOE_H