///
/// Honest "solution": shows that you cannot abstract the conditional expression
///
/// JavaScript uses Strict Evaluation => 
/// it is not possible to create a functional abstraction for the conditional expression
///
/// Conclusion: the conditional expression in JS is "magic"
///

const ifx = (cond, expT, expF) => {
    return cond ? expT : expF;
}

const fibo = (n) => {
    //  return n < 2 ? n : fibo(n-1) + fibo(n-2)   // <= esto funciona
    return ifx(n < 2,  n,  fibo(n-1) + fibo(n-2)); // <= mismas expresiones, pero no funciona
};

///
/// Workaround: transform the code to use closures, to delay evaluation
///
/// Note how this is NOT a functional abstraction of the conditional expression
/// we have to change the expressions (arguments) used in the *caller* (fibonacci)
///

const ifCl = (cond, expT, expF) => {
    return (cond ? expT : expF)(); // selects the closure and applies it
}

const fiboCl = (n) => {
    return ifCl(n < 2, () => n, () => fiboCl(n-1) + fiboCl(n-2));
};
