#[derive(Debug)]
pub enum Error {
    IO(std::io::Error),
    Parse(String)
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IO(e)
    }
}
