#ifndef SRC_STATE_MACHINE_DOOR_H
#define SRC_STATE_MACHINE_DOOR_H

#include <vector>

namespace state_macine_door {

enum class StateDoor { Closed, Open, Locked };

enum class EventDoor { OpenDoor, CloseDoor, LockDoor, UnlockDoor };

class StateMachineDoor {
  public:
  auto RunStateMachineDoor(StateDoor initial, const std::vector<EventDoor>& events) -> StateDoor;
  private:
  auto Transition(StateDoor state, EventDoor event) -> StateDoor;
};
}


#endif // EXACT_SQUARE_ROOT_H