use crate::parse::Position;

#[derive(Debug)]
pub enum Error {
    IO(std::io::Error),
    At(String,Position),
    Msg(String)
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IO(e)
    }
}

impl Error {
    pub fn to_msg(self,input_string: &str) -> Self {
        match self {
            Error::At(reason,pos) => {
                let i = input_string.len() - pos;
                let error = format!("*** {} ***\n{}!!!ERROR!!!{}\n", reason, &input_string[..i], &input_string[i..]);
                Error::Msg(error)
            }
            _ => self
        }
    }
}
