// algorithm Sieve of Eratosthenes is
//     input: an integer n > 1.
//     output: all prime numbers from 2 through n.

//     let A be an array of Boolean values, indexed by integers 2 to n,
//     initially all set to true.
    
//     for i = 2, 3, 4, ..., not exceeding √n do
//         if A[i] is true
//             for j = i2, i2+i, i2+2i, i2+3i, ..., not exceeding n do
//                 A[j] := false

//     return all i such that A[i] is true.

const makeArray = (n) => {
    let a = []
    for (let i=1; i<=n; i++){
        a.push(true)
    }
    a[0] = false, a[1] = false
    return a
}

const sieveOfEratosthenes = (n) => {
    a = makeArray(n)
    for(let i=2; i<n; i++){
        if (a[i] === true)
            for (j=2*i; j<=n; j += i)
                a[j] = false
    }
    a = a.map((val, index) => {return val ? index : val}).filter(val => val !== false)
    console.log(a)
}
// sieveOfEratosthenes(50)


// for (j=2*n; j<=arr.length; j += n)
// arr[j] = false
const makeFalseRec = (arr, index, addNum) => {
    if (index >= arr.length)
        return arr
    
    arr[index] = false
    arr = makeFalseRec(arr,index + addNum, addNum)
    return arr
}

// for(let i=2; i<n; i++){
//     if (a[i] === true)
//        Make false
// }
const sieveRec = (arr, n) => {

    if (n >= Math.sqrt(arr.length)) {
        return arr
    }

    if (arr[n] === true){
        arr = makeFalseRec(arr, 2*n, n)
    }
    arr = sieveRec(arr, n + 1)
    return arr
}


const sieveOfEratosthenesRec = (n) => {
    sieveArr = makeArray(n)
    sieveArr = sieveRec(sieveArr, 2)

    return sieveArr.map((val, index) => {
        return val ? index : val}).filter(val => val !== false)
}
console.log(sieveOfEratosthenesRec(500))
