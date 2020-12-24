extern crate nom;
use nom::{
    IResult,
    bytes::complete::{tag, take_while_m_n},
    combinator::map_res,
    sequence::tuple,
    number::complete::us as c_u8
};

#[derive(Debug,PartialEq)]
pub struct Player {
    pub id: u8,
    pub deck: Vec<u8>,
}

fn c_id(input: &str) -> IResult<&str, u8> {
    map_res(
        tag("Player ")
    )
    
}

fn is_numeric(c: char) -> bool {
    c.is_digit(10)
}

fn c_number(input: &str) -> IResult<&str, u8> {
    
}

fn c_deck(input: &str) -> IResult<&str, Vec<u8>> {
    take_while()
}


fn c_player(input: &str) -> IResult<&str, Player> {
    let (input, id) = c_id(input)?;
    let (input, deck) = c_deck(input)?;
    Ok((input, Player { id, deck }))
}

fn main() {
    println!("Hello, world!");
}
