import asyncio
import websockets

async def handler(websocket):
    print("Client verbunden")
    
    try:
        async for message in websocket:
            print(f"Empfangen: {message}")
            response = f"Echo: {message}"
            await websocket.send(response)
    except websockets.exceptions.ConnectionClosed:
        print("Client getrennt")

async def main():
    async with websockets.serve(handler, "localhost", 8765):
        print("Server läuft auf ws://localhost:8765")
        await asyncio.Future()  # läuft endlos

asyncio.run(main())