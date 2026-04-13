import asyncio
import websockets
import json

async def handler(websocket):
    print("Client verbunden")

    try:
        async for message in websocket:
            data = json.loads(message)

            # Nachrichtentyp prüfen
            if data.get("type") == "user_data":
                vorname = data.get("vorname")
                nachname = data.get("nachname")

                print(f"Empfangen: {vorname} {nachname}")

                response = {
                    "type": "response",
                    "test": "testTest",
                    "vorname": vorname,
                    "nachname": nachname,
                    "message": f"Hallo {vorname} {nachname}"
                }

                await websocket.send(json.dumps(response))

    except websockets.exceptions.ConnectionClosed:
        print("Client getrennt")

async def main():
    async with websockets.serve(handler, "localhost", 8765):
        print("Server läuft auf ws://localhost:8765")
        await asyncio.Future()

asyncio.run(main())