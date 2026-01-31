#ifndef SRC_STATE_MACHINE_TIC_TAC_TOE_H
#define SRC_STATE_MACHINE_TIC_TAC_TOE_H


#include <variant>
#include <array>


namespace state_machine_tic_tac_toe {
/**
 * State PlayerX, PlayerO
 * Event StartGame, NextPlayer, EndGame
 */

class InitGame {
public:
  auto PrintInitGame() -> void;
};

class PlayerX {
public:
  auto GetAreaNumber() -> int;
};

class PlayerO {
public:
  auto GetAreNumber() -> int;
};

class StateMachineTicTacToe{
public:
  StateMachineTicTacToe();
  auto StartGame() -> void;

  auto NextPlayerX()-> void;

  auto NextPlayerO() -> void;

  auto FinishGame() -> void;

  auto AddFeldNumber(int num) -> void;

  auto HasPlayerWon() -> bool;

  private:
    std::variant<InitGame, PlayerX, PlayerO> state_{};
    enum class Player{X, O, N};
    std::array<Player, 9> area_{};
    auto PlayerToString(Player player) -> std::string;
    auto PrintArea() -> void;
};
}
#endif // SRC_STATE_MACHINE_TIC_TAC_TOE_H