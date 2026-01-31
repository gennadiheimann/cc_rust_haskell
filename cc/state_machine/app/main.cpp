#include <iostream>
#include "state_machine_door.h"
#include "state_machine_tic_tac_toe.h"
#include <vector>

auto main() -> int {
  // std::vector<state_macine_door::EventDoor> events {
  //       state_macine_door::EventDoor::OpenDoor,
  //       state_macine_door::EventDoor::CloseDoor,
  //       state_macine_door::EventDoor::LockDoor,
  //       state_macine_door::EventDoor::UnlockDoor,
  //       state_macine_door::EventDoor::OpenDoor
  //   };

  // state_macine_door::StateMachineDoor sm;

  // state_macine_door::StateDoor finalState = sm.RunStateMachineDoor(state_macine_door::StateDoor::Closed, events);

  // std::cout << static_cast<int>(finalState) << std::endl;

  state_machine_tic_tac_toe::StateMachineTicTacToe game{};
  game.StartGame();
}