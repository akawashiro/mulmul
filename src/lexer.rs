fn tokenize(input_s: String) -> Vec<String> {
    let input : Vec<char> = input_s.chars().collect();
    let mut ret = Vec::new();
    let mut pos : usize = 0;
    let mut tok : String = String::new();
    let ops = "+*/-()";
    let spaces = " \n\t";
    loop {
        if pos >= input.len() {
            break
        }
        if input[pos] == '(' && pos < input.len()-1 && input[pos+1] == '*' {
            pos += 2;
            ret.push("(*".to_string());
        } else if input[pos] == '*' && pos < input.len()-1 && input[pos+1] == ')' {
            pos += 2;
            ret.push("*)".to_string());
        } else if input[pos] == '-' && pos < input.len()-1 && input[pos+1] == '>' {
            pos += 2;
            ret.push("->".to_string());
        } else if ops.contains(input[pos]) {
            if "" != tok {
                ret.push(tok);
                tok = String::new();
            }	
            ret.push(input[pos].to_string());
            pos+=1;
        } else if spaces.contains(input[pos]) {
            if "" != tok {
                ret.push(tok);
                tok = String::new();
            }	
            pos+=1;
        }else {
            tok.push(input[pos]);
            pos+=1;
        }
    }
    if "" != tok {
        ret.push(tok);
    }	
    ret
}

pub struct Lexer {
    tokens: Vec<String>,
    position: usize
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let tok = tokenize(input);
        println!("{:?}", tok);
        Lexer { tokens: tok, position: 0 }
    }

    pub fn peek(&mut self) -> String {
        self.tokens[self.position].clone()
    }

    pub fn getone(&mut self) -> String {
        let r = self.tokens[self.position].clone();
        self.position += 1;
        r
    }

    pub fn is_end(&mut self) -> bool {
        self.tokens.len() == self.position
    }
}
