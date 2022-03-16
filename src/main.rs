use std::fmt;
use std::ops::*;
mod numb;
//Содержимое выражения
#[derive(Clone,PartialEq)]
#[allow(dead_code)]
#[allow(non_snake_case)]
enum Expr {
    Var(String), //Переменная
    Num(numb::Numb), //Число или численное выражение
    Exp1(Box<(numb::Act,Expr)>), // Функция одного аргумента
    Exp2(Box<(Expr,numb::Act,Expr)>), //Функция двкх аргументов
}
#[allow(dead_code)]
#[allow(non_snake_case)]
impl Expr {
    fn new() -> Expr {
            Expr::Num(numb::Numb::Som(0.0))
    }
    fn new_var(S : &str) -> Expr {
            Expr::Var(String::from(S))
    }
    fn new_num(f : f64) -> Expr {
            Expr::Num(numb::Numb::Som(f))
    }
    fn new_exp1(a : numb::Act, e1 : Expr) -> Expr {
            Expr::Exp1(Box::new((a,e1)))
    }
    fn new_exp2(e1 : Expr, a : numb::Act, e2 : Expr) -> Expr {
            Expr::Exp2(Box::new((e1,a,e2)))
    }
    fn calc(&self, V : &numb::Variables) -> numb::Numb {
        match self.clone() {
            Expr::Var(s) => {V.get_value(s)}
            Expr::Num(a) => {a}
            Expr::Exp1(e) => {  //Обрабатываем различные действия
                                match e.0 {
                                    numb::Act::Neg => {-e.1.calc(V)}
                                    numb::Act::Sin => {e.1.calc(V).sin()}
                                    numb::Act::Cos => {e.1.calc(V).cos()}
                                    numb::Act::Tan => {e.1.calc(V).tan()}
                                    numb::Act::Abs => {e.1.calc(V).abs()}
                                    numb::Act::ASin => {e.1.calc(V).asin()}
                                    numb::Act::ACos => {e.1.calc(V).acos()}
                                    numb::Act::ATan => {e.1.calc(V).atan()}
                                    _ => {panic!("Can't find such an action for Expr1");}
                                } 
                             }
            Expr::Exp2(e) => {  //Обрабатываем различные действия
                                match e.1 {
                                    numb::Act::Add => {e.0.calc(V) + e.2.calc(V)}
                                    numb::Act::Sub => {e.0.calc(V) - e.2.calc(V)}
                                    numb::Act::Mul => {e.0.calc(V) * e.2.calc(V)}
                                    numb::Act::Div => {e.0.calc(V) / e.2.calc(V)}
                                    numb::Act::Pow => {e.0.calc(V).powf(e.2.calc(V))}
                                    numb::Act::Log => {e.0.calc(V).log(e.2.calc(V))}
                                    _ => {panic!("Can't find such an action for Expr2");}
                                }
                            }
            }
    }
    // - на - дает +
    // умножение или деление на 1 не прописывается
    // слогаемые в суммах перераспределяются с начала переменные и функции с ними - потом 
    // Множители в произведениях перераспределяются так-же
    // Одинаковые члены в числителе и знаменателе не вычеркиваются для того, что-бы не потерять Nan значения
    fn simplify(&self) -> Expr {
        match self.clone() {
            Expr::Var(_) => {self.clone()}
            Expr::Num(_) => {self.clone()}
            Expr::Exp1(e) =>    {  //Обрабатываем различные действия
                            if self.is_culcable() {
                                let V = numb::Variables::new();
                                Expr::Num(self.calc(&V))
                            } else {
                                match e.0 {
                                    numb::Act::Neg => {
                                        match e.1 {
                                            Expr::Num(n) => {
                                                match n {
                                                    numb::Numb::Som(N) => {if N < 0.0 {Expr::new_num(-N)} else {self.clone()}}
                                                    numb::Numb::Nan => {self.clone()}
                                                }
                                            }
                                            Expr::Exp1(q) => {
                                                match q.0 {
                                                    numb::Act::Neg => {q.1}
                                                    _ => {self.clone()}
                                                }
                                            }
                                            _ => {self.clone()}
                                        }
                                    }
                                    _ => {Expr::new_exp1(e.0,e.1.simplify())}
                                }
                            }
                                }
            Expr::Exp2(e) =>    {  //Обрабатываем различные действия
                            if self.is_culcable() {
                                let V = numb::Variables::new();
                                Expr::Num(self.calc(&V))
                            } else {
                                match e.1 {
                                    numb::Act::Add => {self.simplify_summ()}
                                    numb::Act::Sub => {self.simplify_summ()}
                                    numb::Act::Mul => {e.0 * e.2}
                                    numb::Act::Div => {e.0 / e.2}
                                    numb::Act::Pow => {Expr::new_exp2(e.0.simplify(),numb::Act::Pow,e.2.simplify())}
                                    numb::Act::Log => {Expr::new_exp2(e.0.simplify(),numb::Act::Log,e.2.simplify())}
                                    _ => {panic!("Can't find such an action for Expr2");}
                                }
                            }
                                }
        }
    }
    //Применяем данную функцию для упрощения суммы. В ней мы проходим по дереву и рассматриваем все
    //элементы суммы, группируем их на вычислимые выражение и не вычислимые выражения. После чего
    //складываем все вычислимые выражения и преобразуем в выражение (не вычислимое) + (вычислимое)
    fn simplify_summ(&self) -> Expr {
        match self.clone() {
            Expr::Exp2(e) => {
                let mut uncalcuble = Expr::new();
                let mut calcuble = Expr::new();
                match e.1.clone() {
                    numb::Act::Add | numb::Act::Sub => {
                        //Рекурсивно вызываем функцию
                        match e.0.clone() {
                            Expr::Var(_) => {uncalcuble += e.0;}
                            Expr::Num(_) => {calcuble += e.0;}
                            Expr::Exp1(_) => {if e.0.is_culcable() {calcuble += e.0;} else {uncalcuble += e.0;}}
                            Expr::Exp2(a) => {
                                if e.0.is_culcable() {calcuble += e.0;} else {
                                match a.1 {
                                    numb::Act::Add | numb::Act::Sub => {
                                        let A = e.0.simplify_summ();
                                        match A {
                                            Expr::Exp2(q) => {
                                                uncalcuble += q.0;
                                                calcuble += q.2;
                                            }
                                            _ => {}
                                        }
                                    }
                                    _ => {if e.0.is_culcable() {calcuble += e.0;} else {uncalcuble += e.0;}}
                                }}
                            }
                        }
                        match e.1.clone() {
                            numb::Act::Add => {
                                match e.2.clone() {
                                Expr::Var(_) => {uncalcuble += e.2;}
                                Expr::Num(_) => {calcuble += e.2;}
                                Expr::Exp1(_) => {if e.2.is_culcable() {calcuble += e.2.simplify();} else {uncalcuble += e.2.simplify();}}
                                Expr::Exp2(a) => {
                                    if e.2.is_culcable() {calcuble += e.2;} else {
                                    match a.1 {
                                        numb::Act::Add | numb::Act::Sub => {
                                            let A = e.2.simplify_summ();
                                            match A {
                                                Expr::Exp2(q) => {
                                                    uncalcuble += q.0;
                                                    calcuble += q.2;
                                                }
                                                _ => {}
                                            }
                                        }
                                        _ => {if e.2.is_culcable() {calcuble += e.2.simplify();} else {uncalcuble += e.2.simplify();}}
                                    }}
                                }
                            }}
                            numb::Act::Sub => {
                                match e.2.clone() {
                                Expr::Var(_) => {uncalcuble -= e.2;}
                                Expr::Num(_) => {calcuble -= e.2;}
                                Expr::Exp1(_) => {if e.2.is_culcable() {calcuble -= e.2.simplify();} else {uncalcuble -= e.2.simplify();}}
                                Expr::Exp2(a) => {
                                    if e.2.is_culcable() {calcuble -= e.2;} else {
                                    match a.1 {
                                        numb::Act::Add | numb::Act::Sub => {
                                            let A = e.2.simplify_summ();
                                            match A {
                                                Expr::Exp2(q) => {
                                                    uncalcuble -= q.0;
                                                    calcuble -= q.2;
                                                }
                                                _ => {}
                                            }
                                        }
                                        _ => {if e.2.is_culcable() {calcuble -= e.2.simplify();} else {uncalcuble -= e.2.simplify();}}
                                    }}
                                }
                            }}
                        _ => {}
                        }}
                    _ => {panic!("Tried to use simplify_summ to non summ  expression")}
                }
                let V = numb::Variables::new();
                println!("{:#?} , {:#?}",calcuble,uncalcuble);
                calcuble = transform_plus(calcuble);
                uncalcuble = transform_plus(uncalcuble);
                println!("{:#?} , {:#?}",calcuble,uncalcuble);
                if calcuble == Expr::new() {return uncalcuble;}
                if uncalcuble == Expr::new() {return Expr::Num(calcuble.calc(&V));}
                uncalcuble + Expr::Num(calcuble.calc(&V))
            }
            _ => {panic!("Tried to use simplify_summ to non summ  expression")}
        }
    }
    //Применяем данную функцию для упрощения произведения. В ней мы проходим по дереву и рассматриваем все
    //элементы произведения, группируем их на вычислимые выражение и не вычислимые выражения. После чего
    //выполняем произведения всех вычислимых выражений и преобразуем в выражение (не вычислимое) * (вычислимое)
    //после этого можем использовать вычеркивание деления на нули и еденицы
    fn simplify_product(&self) -> Expr {
        match self.clone() {
            Expr::Exp2(e) => {
                let mut uncalcuble = Expr::new_num(1.0);
                let mut calcuble = Expr::new_num(1.0);
                match e.1.clone() {
                    numb::Act::Mul | numb::Act::Div => {
                        //Рекурсивно вызываем функцию
                        match e.0.clone() {
                            Expr::Var(_) => {uncalcuble *= e.0;}
                            Expr::Num(_) => {calcuble *= e.0;}
                            Expr::Exp1(_) => {if e.0.is_culcable() {calcuble *= e.0;} else {uncalcuble *= e.0;}}
                            Expr::Exp2(a) => {
                                match a.1 {
                                    numb::Act::Mul | numb::Act::Div => {
                                        let A = e.0.simplify_product();
                                        match A {
                                            Expr::Exp2(q) => {
                                                uncalcuble *= q.0;
                                                calcuble *= q.2;
                                            }
                                            _ => {}
                                        }
                                    }
                                    _ => {if e.0.is_culcable() {calcuble *= e.0;} else {uncalcuble *= e.0;}}
                                }
                            }
                        }
                        match e.1.clone() {
                            numb::Act::Mul => {
                                match e.2.clone() {
                                Expr::Var(_) => {uncalcuble *= e.2;}
                                Expr::Num(_) => {calcuble *= e.2;}
                                Expr::Exp1(_) => {if e.2.is_culcable() {calcuble *= e.2.simplify();} else {uncalcuble *= e.2.simplify();}}
                                Expr::Exp2(a) => {
                                    match a.1 {
                                        numb::Act::Mul | numb::Act::Div => {
                                            let A = e.2.simplify_product();
                                            match A {
                                                Expr::Exp2(q) => {
                                                    uncalcuble *= q.0;
                                                    calcuble *= q.2;
                                                }
                                                _ => {}
                                            }
                                        }
                                        _ => {if e.2.is_culcable() {calcuble *= e.2.simplify();} else {uncalcuble *= e.2.simplify();}}
                                    }
                                }
                            }}
                            numb::Act::Div => {
                                match e.2.clone() {
                                Expr::Var(_) => {uncalcuble /= e.2;}
                                Expr::Num(_) => {calcuble /= e.2;}
                                Expr::Exp1(_) => {if e.2.is_culcable() {calcuble /= e.2.simplify();} else {uncalcuble /= e.2.simplify();}}
                                Expr::Exp2(a) => {
                                    match a.1 {
                                        numb::Act::Mul | numb::Act::Div => {
                                            let A = e.2.simplify_product();
                                            match A {
                                                Expr::Exp2(q) => {
                                                    uncalcuble /= q.0;
                                                    calcuble /= q.2;
                                                }
                                                _ => {}
                                            }
                                        }
                                        _ => {if e.2.is_culcable() {calcuble /= e.2.simplify();} else {uncalcuble /= e.2.simplify();}}
                                    }
                                }
                            }}
                        _ => {}
                        }}
                    _ => {panic!("Tried to use simplify_summ to non summ  expression")}
                }
                let V = numb::Variables::new();
                //println!("{:#?} , {:#?}",calcuble,uncalcuble);
                //calcuble = transform_plus(calcuble);
                //uncalcuble = transform_plus(uncalcuble);
                //println!("{:#?} , {:#?}",calcuble,uncalcuble);
                //if calcuble == Expr::new() {return uncalcuble;}
                //if uncalcuble == Expr::new() {return Expr::Num(calcuble.calc(&V));}
                uncalcuble + Expr::Num(calcuble.calc(&V))
            }
            _ => {panic!("Tried to use simplify_summ to non summ  expression")}
        }
    }

    //Провряем, является ли выражение вычислимым без обращения к переменным
    fn is_culcable(&self) -> bool {
        match self.clone() {
            Expr::Var(_) => {false}
            Expr::Num(_) => {true}
            Expr::Exp1(e) =>    {  //Обрабатываем различные действия
                            e.1.is_culcable()
                                }
            Expr::Exp2(e) =>    {  //Обрабатываем различные действия
                            e.0.is_culcable() && e.2.is_culcable()
                                }
        }
    }
}

//Уменьшаем структуру 
fn transform_plus(c : Expr) -> Expr {
    match c {
        Expr::Var(_) => {Expr::new()}
        Expr::Num(_) => {Expr::new()}
        Expr::Exp1(_) => {Expr::new()}
        Expr::Exp2(e) => {
            match e.0 {
                Expr::Var(_) => {Expr::new()}
                Expr::Num(_) => {e.2}
                Expr::Exp1(_) => {Expr::new()}
                Expr::Exp2(c) => {
                    c.2 + e.2
                }
            }
        }
    }
}

impl Add for Expr {
    type Output = Self;
    fn add(self, other: Expr) -> Expr {
        Expr::new_exp2(self, numb::Act::Add, other)
    }
} 

impl AddAssign for Expr {
    fn add_assign(&mut self, other: Expr) {
        *self = self.clone() + other;
    }
}

impl Sub for Expr {
    type Output = Self;
    fn sub(self, other: Expr) -> Expr {
        Expr::new_exp2(self, numb::Act::Sub, other)
    }
}

impl SubAssign for Expr {
    fn sub_assign(&mut self, other: Expr) {
        *self = self.clone() - other;
    }
}

impl Mul for Expr {
    type Output = Self;
    fn mul(self, other: Expr) -> Expr {
        Expr::new_exp2(self, numb::Act::Mul, other)
    }
}

impl MulAssign for Expr {
    fn mul_assign(&mut self, other: Expr) {
        *self = self.clone() * other;
    }
}

impl Div for Expr {
    type Output = Self;
    fn div(self, other: Expr) -> Expr {
        Expr::new_exp2(self, numb::Act::Div, other)
    }
}

impl DivAssign for Expr {
    fn div_assign(&mut self, other: Expr) {
        *self = self.clone() / other;
    }
}

impl Neg for Expr {
    type Output = Self;
    fn neg(self) -> Expr {
        Expr::new_exp1(numb::Act::Neg, self)
    }
}

fn warp(s : String) -> String {
    (String::from("(") + &s[..]) + ")"
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.clone() {
            Expr::Var(s) => {write!(f, "{}", s)?;}
            Expr::Num(a) => {write!(f, "{}", a.to_string())?;}
            Expr::Exp1(e) => {  //Обрабатываем различные действия
                                match e.1.clone() {
                                Expr::Num(a) => { //Добавлена обработка случая --1 -> 1
                                    if e.0 == numb::Act::Neg {
                                        match a {
                                            numb::Numb::Som(b) => if b < 0.0 {write!(f, "{}", -b)?;} else {write!(f, "{}{}", e.0,e.1)?;}
                                            _ => {write!(f, "{}", a)?;}
                                        }
                                    }}
                                Expr::Var(_) => {write!(f, "{}{}", e.0,e.1)?;}
                                Expr::Exp1(_) => {write!(f, "{}{}", e.0,e.1)?;}
                                Expr::Exp2(_) => { match e.1.clone() {
                                                        Expr::Exp2(a) => {
                                                            match a.1 {
                                                                numb::Act::Add | numb::Act::Sub => {write!(f, "{}{}", e.0,warp(e.1.to_string()))?;}
                                                                _ => {write!(f, "{}{}", e.0,e.1)?;}
                                                            }}
                                                        _ => {panic!();}
                            }}}}
            Expr::Exp2(e) => {  //Обрабатываем различные действия
                                match e.1 {
                                   numb::Act::Log | numb::Act::Pow => {
                                           write!(f, "{}{}{}", warp(e.0.to_string()),e.1,warp(e.2.to_string()))?;
                                       }
                                   numb::Act::Mul => {
                                        let mut E0 = e.0.clone().to_string();
                                        let mut E2 = e.2.clone().to_string();
                                        match e.0 {
                                            Expr::Exp2(q) => {
                                                if q.1 == numb::Act::Add || q.1 == numb::Act::Sub {E0 = warp(E0);}
                                            }
                                            _ => {}
                                        }
                                        match e.2 {
                                            Expr::Exp2(q) => {
                                                if q.1 == numb::Act::Add || q.1 == numb::Act::Sub {E2 = warp(E2);}
                                            }
                                            _ => {}
                                        }

                                        write!(f, "{}{}{}",E0,e.1,E2)?;
                                   }
                                   numb::Act::Div => {
                                        let mut E0 = e.0.clone().to_string();
                                        let mut E2 = warp(e.2.clone().to_string());
                                        match e.0 {
                                            Expr::Exp2(q) => {
                                                if q.1 == numb::Act::Add || q.1 == numb::Act::Sub {E0 = warp(E0);}
                                            }
                                            _ => {}
                                        }
                                        write!(f, "{}{}{}",E0,e.1,E2)?;
                                   }
                                   _ => {write!(f, "{}{}{}",e.0,e.1,e.2)?;}
                                }
                             }
            }
        Ok(())
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.clone() {
            Expr::Var(s) => {write!(f, "{}", s)?;}
            Expr::Num(a) => {write!(f, "{}", a.to_string())?;}
            Expr::Exp1(e) => {write!(f, "{}{}",e.0,warp(e.1.to_string()))?;}
            Expr::Exp2(e) => {write!(f, "{}{}{}", warp(e.0.to_string()),e.1,warp(e.2.to_string()))?;}
            }
        Ok(())
    }
}


//✅Сделать массив переменных не глобальным, а отдельным, после чего ввести его в функции - в
//релизной типа версии

//Доделать определитель для Nan чисел при степени и логарифме, полным это будет лишь в случае, если
//числа смогут становиться комплексными, но пока такого не будет

//✅calc - вычислить - работает - задача - попробовать перевести в быстро вычисляемый формат, но это
//врядли возможно

//✅simplify - упращение выражения, которое преобразует его в другое выражение с тем же выводом. -
// пробовать делить выражение на полностью вычислимые и не вычислимые ветви, содержащие переменные
// - получится рекурсивная функция упрощения

//nfindroot - численно найти корни. Считается, что уравнение равно нулю может быть так-же использовано
//с параметром метода численного решения. Newton - уравнение будет численно решаться методом
//Ньютона
//Binom - проверяем большое число точек на то, лежат ли они выше или ниже нуля. Если находим такое
//отрезок, то на нем ищем точку. Можно задать точку, с которой будет начинаться поиск и шаг
//алгоритма

//diff - производная -> выражение
//integrate - интеграл -> выражение

//ndiff - численная производная в точке -> число выбираем точки в окрестности данной и считаем
//производную
//nintegrate - численный интеграл -> число
//подаем функции на вход начальное и конечное значение и шаг, после чего численно считаем интегра

fn main() {
    let mut V = numb::Variables::new();
    V.set_var("x",2.0);
    let mut E = Expr::new_exp2( Expr::new_exp2(Expr::new_num(1.0),numb::Act::Add,Expr::new_num(2.0)),
                                numb::Act::Add,
                                Expr::new_exp2(Expr::new_var("x"),numb::Act::Mul,Expr::new_num(2.0)));
    println!("{}",E);
    E = E.simplify();
    println!("{}",E);
}

//Тесты для метода simplify Expr
#[test]
fn simplify_expr_01() {
    assert_eq!(Expr::new_num(1.0).simplify(), Expr::new_num(1.0));
}
#[test]
fn simplify_expr_02() {
    assert_eq!((Expr::new_num(1.0) + Expr::new_num(1.0) + Expr::new_var("x")).simplify(), Expr::new_var("x") + Expr::new_num(2.0));
}
#[test]
fn simplify_expr_03() {
    assert_eq!((Expr::new_num(1.0) + Expr::new_num(1.0)).simplify(), Expr::new_num(2.0));
}
#[test]
fn simplify_expr_04() {
    assert_eq!((Expr::new_num(1.0) - Expr::new_num(1.0)).simplify(), Expr::new_num(0.0));
}
#[test]
fn simplify_expr_05() {
    assert_eq!(( Expr::new_num(1.0) - Expr::new_num(2.0) + Expr::new_var("x")).simplify(), - Expr::new_var("x") - Expr::new_num(2.0));
}


/*
//Тесты для трейта Debug для Expr
#[test]
fn Display_Expr_01() {
    assert_eq!(Expr::new_num(1.0).to_string(), "1");
}
#[test]
fn Display_Expr_02() {
    assert_eq!(Expr::new_num(1.5).to_string(), "1.5");
}
#[test]
fn Display_Expr_03() {
    assert_eq!(Expr::new_exp2(Expr::new_var("x"),numb::Act::Add,Expr::new_var("y")).to_string(), "x+y");
}
#[test]
fn Display_Expr_04() {
    assert_eq!(Expr::new_exp2(Expr::new_exp2(Expr::new_num(1.0),numb::Act::Mul,Expr::new_num(1.0)),numb::Act::Add,Expr::new_var("y")).to_string(), "1*1+y");
}
#[test]
fn Display_Expr_06() {
    assert_eq!(Expr::new_exp2(Expr::new_exp2(Expr::new_num(1.0),numb::Act::Add,Expr::new_num(1.0)),numb::Act::Mul,Expr::new_var("y")).to_string(), "(1+1)*y");
}
#[test]
fn Display_Expr_07() {
    assert_eq!(Expr::new_exp2(Expr::new_var("y"),numb::Act::Div,Expr::new_exp2(Expr::new_num(1.0),numb::Act::Add,Expr::new_num(1.0))).to_string(), "y/(1+1)");
}
#[test]
fn Display_Expr_08() {
    assert_eq!( Expr::new_exp2(
                    Expr::new_exp2(Expr::new_num(1.0),numb::Act::Add,Expr::new_num(2.0)),
                    numb::Act::Div,
                    Expr::new_exp2(Expr::new_num(3.0),numb::Act::Add,Expr::new_num(4.0))).to_string(), "(1+2)/(3+4)");
}
*/
/*
//Тесты для проверки работы функции calc
#[test]
fn calc_test_01_num() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_num(1.0).calc(&V), numb::Numb::Som(1.0));
}
#[test]
fn calc_test_02_var() {
    let mut V = numb::Variables::new();
    V.set_var("x",1.0);
    assert_eq!(Expr::new_var("x").calc(&V), numb::Numb::Som(1.0));
}
//Проверка унарных операторов для expr1
#[test]
fn calc_test_03_neg() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_num(1.0).calc(&V), numb::Numb::Som(1.0));
}
#[test]
fn calc_test_04_sin() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp1(numb::Act::Sin,Expr::new_num(1.0)).calc(&V), numb::Numb::Som(1.0f64.sin()));
}
#[test]
fn calc_test_05_cos() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp1(numb::Act::Cos,Expr::new_num(1.0)).calc(&V), numb::Numb::Som(1.0f64.cos()));
}
#[test]
fn calc_test_06_tan() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp1(numb::Act::Tan,Expr::new_num(1.0)).calc(&V), numb::Numb::Som(1.0f64.tan()));
}
#[test]
fn calc_test_07_abs() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp1(numb::Act::Abs,Expr::new_num(-1.0)).calc(&V), numb::Numb::Som((-1.0f64).abs()));
}
#[test]
fn calc_test_08_asin1() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp1(numb::Act::ASin,Expr::new_num(-1.0)).calc(&V), numb::Numb::Som((-1.0f64).asin()));
}
#[test]
fn calc_test_09_asin2() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp1(numb::Act::ASin,Expr::new_num(-2.0)).calc(&V), numb::Numb::Nan);
}
#[test]
fn calc_test_10_acos() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp1(numb::Act::ACos,Expr::new_num(-1.0)).calc(&V), numb::Numb::Som((-1.0f64).acos()));
}
#[test]
fn calc_test_11_acos2() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp1(numb::Act::ACos,Expr::new_num(-2.0)).calc(&V), numb::Numb::Nan);
}
#[test]
fn calc_test_12_atan() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp1(numb::Act::ATan,Expr::new_num(-2.0)).calc(&V), numb::Numb::Som((-2.0f64).atan()))
}
#[test]
fn calc_test_13_add() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp2(Expr::new_num(2.0),numb::Act::Add,Expr::new_num(2.0)).calc(&V), numb::Numb::Som(4.0))
}
#[test]
fn calc_test_14_sub() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp2(Expr::new_num(2.0),numb::Act::Sub,Expr::new_num(2.0)).calc(&V), numb::Numb::Som(0.0))
}
#[test]
fn calc_test_15_mul() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp2(Expr::new_num(2.0),numb::Act::Mul,Expr::new_num(2.0)).calc(&V), numb::Numb::Som(4.0))
}
#[test]
fn calc_test_16_div() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp2(Expr::new_num(2.0),numb::Act::Div,Expr::new_num(2.0)).calc(&V), numb::Numb::Som(1.0))
}
#[test]
fn calc_test_17_pow() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp2(Expr::new_num(3.0),numb::Act::Pow,Expr::new_num(3.0)).calc(&V), numb::Numb::Som(27.0))
}
#[test]
fn calc_test_18_log() {
    let V = numb::Variables::new();
    assert_eq!(Expr::new_exp2(Expr::new_num(4.0),numb::Act::Log,Expr::new_num(2.0)).calc(&V), numb::Numb::Som(2.0))
}
*/
/*
//Тесты на проверку работы операций над выражениями
#[test]
fn add_expr() {
    assert_eq!(Expr::new_num(1.0) + Expr::new_num(2.0), Expr::new_exp2(Expr::new_num(1.0),numb::Act::Add,Expr::new_num(2.0)));
}
#[test]
fn sub_expr() {
    assert_eq!(Expr::new_num(1.0) - Expr::new_num(2.0), Expr::new_exp2(Expr::new_num(1.0),numb::Act::Sub,Expr::new_num(2.0)));
}
#[test]
fn mul_expr() {
    assert_eq!(Expr::new_num(1.0) * Expr::new_num(2.0), Expr::new_exp2(Expr::new_num(1.0),numb::Act::Mul,Expr::new_num(2.0)));
}
#[test]
fn div_expr() {
    assert_eq!(Expr::new_num(1.0) / Expr::new_num(2.0), Expr::new_exp2(Expr::new_num(1.0),numb::Act::Div,Expr::new_num(2.0)));
}
#[test]
fn neg_expr() {
    assert_eq!(- Expr::new_num(2.0), Expr::new_exp1(numb::Act::Neg,Expr::new_num(2.0)));
}
*/
/*
//Тесты на проверку работы чисел
#[test]
fn add_numb() {
    assert_eq!(numb::Numb::Som(0.0) + numb::Numb::Nan, numb::Numb::Nan);
}

#[test]
#[should_panic]
fn add_numb_panic() {
    assert_eq!(numb::Numb::Som(0.0) + numb::Numb::Nan, numb::Numb::Som(0.0));
}

#[test]
fn sub_numb() {
    assert_eq!(numb::Numb::Som(0.0) - numb::Numb::Nan, numb::Numb::Nan);
}

#[test]
#[should_panic]
fn sub_numb_panic() {
    assert_eq!(numb::Numb::Som(0.0) - numb::Numb::Nan, numb::Numb::Som(0.0));
}

#[test]
fn mul_numb() {
    assert_eq!(numb::Numb::Som(0.0) * numb::Numb::Nan, numb::Numb::Nan);
}

#[test]
#[should_panic]
fn mul_numb_panic() {
    assert_eq!(numb::Numb::Som(0.0) * numb::Numb::Nan, numb::Numb::Som(0.0));
}

#[test]
fn div_numb() {
    assert_eq!(numb::Numb::Som(0.0) / numb::Numb::Nan, numb::Numb::Nan);
}

#[test]
#[should_panic]
fn div_numb_panic() {
    assert_eq!(numb::Numb::Som(0.0) / numb::Numb::Nan, numb::Numb::Som(0.0));
}

#[test]
fn negative_numb_to_string() {
    assert_eq!(numb::Numb::Som(-2.0).to_string(), "-2");
}*/
