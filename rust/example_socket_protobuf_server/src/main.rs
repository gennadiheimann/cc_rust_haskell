use tokio::net::TcpListener;
use tokio::io::AsyncReadExt;
use bytes::BytesMut;
use prost::Message;

pub mod hello_world {
    include!(concat!(env!("OUT_DIR"), "/hello_world.rs"));
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:4000").await?;
    println!("Server wartet auf Nachricht...");

    let (mut socket, _) = listener.accept().await?;
    let mut buf = BytesMut::with_capacity(1024);
    socket.read_buf(&mut buf).await?;

    match hello_world::ChatMessage::decode(buf.freeze()) {
        Ok(chat_msg) => {
            println!(
                "Empfangen: {} (id={})",
                chat_msg.text, chat_msg.id
            );
        }
        Err(e) => println!("Decode error: {:?}", e),
    }
    Ok(())
}
