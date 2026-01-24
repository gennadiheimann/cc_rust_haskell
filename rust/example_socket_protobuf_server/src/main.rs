use tokio::net::TcpListener;
use tokio::io::AsyncReadExt;
use bytes::BytesMut;
use prost::Message;

pub mod demo {
    include!(concat!(env!("OUT_DIR"), "/demo.rs"));
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:4000").await?;
    println!("Server wartet auf Nachricht...");

    let (mut socket, _) = listener.accept().await?;
    
        // 1. Längenpräfix lesen (4 Bytes)
    let mut len_buf = [0u8; 4];
    socket.read_exact(&mut len_buf).await?;
    let msg_len = u32::from_be_bytes(len_buf) as usize;

    // 2. Nachricht lesen
    let mut buf = vec![0u8; msg_len];
    socket.read_exact(&mut buf).await?;

    // 3. Protobuf dekodieren
    match demo::ChatMessage::decode(&*buf) {
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
