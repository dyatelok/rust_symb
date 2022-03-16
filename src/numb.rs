use std::fmt;
use std::ops::*;
use std::collections::HashMap;

#[derive(Clone,PartialEq,Eq)]
#[allow(dead_code)]
pub enum Act {
    //Функции двух аргументов
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Log,
    //Функции одного аргумента
    Neg,
    Sin,
    Cos,
    Tan,
    Ctg,
    Abs,
    ASin,
    ACos,
    ATan,
    ACtg,
}

impl fmt::Display for Act {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
    //Функции двух аргументов
        Act::Add => {write!(f, "+")?;}
        Act::Sub => {write!(f, "-")?;}
        Act::Mul => {write!(f, "*")?;}
        Act::Div => {write!(f, "/")?;}
        Act::Pow => {write!(f, "^")?;}
        Act::Log => {write!(f, "Log")?;}
    //Функции одного аргумента
        Act::Neg => {write!(f, "-")?;}
        Act::Sin => {write!(f, "Sin")?;}
        Act::Cos => {write!(f, "Cos")?;}
        Act::Tan => {write!(f, "Tan")?;}
        Act::Abs => {write!(f, "Abs")?;}
        Act::ASin => {write!(f, "ASin")?;}
        Act::ACos => {write!(f, "ACos")?;}
        Act::ATan => {write!(f, "ATan")?;}
   _ => {panic!("Unable to display operator");}
        }
        Ok(())
    }
}

pub enum Variables {
    Vars(HashMap<String,Numb>)
}

#[allow(non_snake_case)]
#[allow(dead_code)]
impl Variables {
    pub fn new() -> Variables {
        Variables::Vars(HashMap::new())
    }
    pub fn add_value(&mut self,S : String, f : Numb) {
        match self {
            Variables::Vars(v) => {v.insert(S,f);}
        }
    }
    pub fn get_value(&self,S : String) -> Numb {
        match self {
            Variables::Vars(v) => v[&S]
        }
    }
    pub fn add_var(&mut self,S : &str) {
        self.add_value(String::from(S),Numb::Nan);
    }
    pub fn set_var(&mut self,S : &str, val : f64) {
        self.add_value(String::from(S),Numb::Som(val));
    }
}
  
#[derive(Clone,Copy,PartialEq)]
pub enum Numb {
    Som(f64),
    Nan,
} 

impl fmt::Display for Numb {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.clone() {
            Numb::Som(a) => {write!(f,"{}",a);}
            Numb::Nan => {write!(f,"Nan");}
        }
        Ok(())
    }
}

impl fmt::Debug for Numb {
   fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.clone() {
            Numb::Som(a) => {write!(f,"Som({})",a);}
            Numb::Nan => {write!(f,"Nan");}
        }
        Ok(())
    }
} 

impl Add for Numb {
    type Output = Self;
    fn add(self, other: Numb) -> Numb {
        match self {
            Numb::Som(a) => {
                match other {
                    Numb::Som(b) => {Numb::Som(a+b)}
                    Numb::Nan => {Numb::Nan}
                }
            }
            Numb::Nan => {Numb::Nan}
        }
    }
} 
 
impl Sub for Numb {
    type Output = Self;
    fn sub(self, other: Numb) -> Numb {
        match self {
            Numb::Som(a) => {
                match other {
                    Numb::Som(b) => {Numb::Som(a-b)}
                    Numb::Nan => {Numb::Nan}
                }
            }
            Numb::Nan => {Numb::Nan}
        }
    }
} 
 
impl Mul for Numb {
    type Output = Self;
    fn mul(self, other: Numb) -> Numb {
        match self {
            Numb::Som(a) => {
                match other {
                    Numb::Som(b) => {Numb::Som(a * b)}
                    Numb::Nan => {Numb::Nan}
                }
            }
            Numb::Nan => {Numb::Nan}
        }
    }
} 
 
impl Div for Numb {
    type Output = Self;
    fn div(self, other: Numb) -> Numb {
        match self {
            Numb::Som(a) => {
                match other {
                    Numb::Som(0.0) => {Numb::Nan}
                    Numb::Som(b) => {Numb::Som(a / b)}
                    Numb::Nan => {Numb::Nan}
                }
            }
            Numb::Nan => {Numb::Nan}
        }
    }
} 

impl Neg for Numb {
    type Output = Self;
    fn neg(self) -> Numb {
        match self {
            Numb::Som(b) => {Numb::Som(-b)}
            Numb::Nan => {Numb::Nan}
        }
    }
} 
 
impl Numb {
    pub fn sin(&self) -> Numb {
        match self {
            Numb::Som(a) => {Numb::Som(a.sin())}
            Numb::Nan => {Numb::Nan}
        }
    }
    pub fn asin(&self) -> Numb {
        match self {
            Numb::Som(a) => {
                if (a.clone() < -1.0) || (1.0 < a.clone()) {Numb::Nan} else {Numb::Som(a.asin())}
            }
            Numb::Nan => {Numb::Nan}
        }
    }
    pub fn cos(&self) -> Numb {
        match self {
            Numb::Som(a) => {Numb::Som(a.cos())}
            Numb::Nan => {Numb::Nan}
        }
    }
    pub fn acos(&self) -> Numb {
        match self {
            Numb::Som(a) => {
                if (a.clone() < -1.0) || (1.0 < a.clone()) {Numb::Nan} else {Numb::Som(a.acos())}
            }
            Numb::Nan => {Numb::Nan}
        }
    }
    pub fn tan(&self) -> Numb {
        match self {
            Numb::Som(a) => {Numb::Som(a.tan())}
            Numb::Nan => {Numb::Nan}
        }
    }
    pub fn atan(&self) -> Numb {
        match self {
            Numb::Som(a) => {Numb::Som(a.atan())}
            Numb::Nan => {Numb::Nan}
        }
    }
    pub fn abs(&self) -> Numb {
        match self {
            Numb::Som(a) => {Numb::Som(a.abs())}
            Numb::Nan => {Numb::Nan}
        }
    }
    pub fn powf(&self,n : Numb) -> Numb {
        match self {
            Numb::Som(a) => {
                    match n {
                        Numb::Som(b) => {
                            Numb::Som(a.powf(b))
                        }
                        Numb::Nan => {Numb::Nan}
                    }
                }
            Numb::Nan => {Numb::Nan}
        }
    }
    pub fn log(&self,n : Numb) -> Numb {
        match self {
            Numb::Som(a) => {
                    match n {
                        Numb::Som(b) => {Numb::Som(a.log(b))}
                        Numb::Nan => {Numb::Nan}
                    }
                }
            Numb::Nan => {Numb::Nan}
        }
    }
} 
