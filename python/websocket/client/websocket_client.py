import asyncio
import websockets

async def client():
    uri = "ws://localhost:8765"
    
    async with websockets.connect(uri) as websocket:
        await websocket.send("Hallo Server!")
        response = await websocket.recv()
        print(f"Antwort: {response}")

asyncio.run(client())