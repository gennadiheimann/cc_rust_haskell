#include "state_machine_door.h"
#include <iostream>
namespace state_macine_door {
  auto StateMachineDoor::Transition(StateDoor state, EventDoor event) -> StateDoor {
    switch (state) {
      case StateDoor::Closed: {
        if(event == EventDoor::OpenDoor){
          std::cout << "Change to Open" << std::endl;  
          return StateDoor::Open;
        }
        if(event == EventDoor::LockDoor) {
          std::cout << "Change to LookDoor" << std::endl;
          return StateDoor::Closed;
        }
        break;
      }
      case StateDoor::Open: {
        if(event == EventDoor::CloseDoor) {
          std::cout << "Change to Closed" << std::endl;
          return StateDoor::Closed;
        }
        break;
      }
      case StateDoor::Locked: {
        if(event == EventDoor::UnlockDoor) {
          std::cout << "Change to UnlockDoor" << std::endl;
          return StateDoor::Closed;
        }
        break;
      }
    }
    return state; // transition do not change state
  }

  auto StateMachineDoor::RunStateMachineDoor(StateDoor initial, const std::vector<EventDoor>& events) -> StateDoor{
    StateDoor current = initial;
    for (const auto& e : events) {
        current = Transition(current, e);
    }
    return current;
  }
}