// Regex used to locate statements
let regex = /\([\w<->]*[^\(\)][\w<->]*\)/i

// Takes a string, runs it recursively through Tseytin logic
// Adds brackets and returns a string of all items in the array,
// joined with ^
const TseytinTransform = (input) => 
    Tseytin([input])
    .map(item => `(${item})`)
    .join('^')

// Dynamically creating new variable names 
// (arr.len - 1 because sometime we need the same name
// after we've pushed an item)
const getNewVarName = (arr, forBiimplication) => 
    forBiimplication ? "x" + arr.length : "x" + (arr.length - 1)

// Get the statement we need to add to the array 
const getSecondBiimplication = (arr) => 
    `${getNewVarName(arr,true)}<->${arr[0].match(regex)[0]}`

const Tseytin = (arr) => {
    // Generally a statement with less or equal than 5 symbols will be
    // Example : (`x4)  (` is a negation)
    // As long as there are brackets, this should be a good base case
    if (arr[0].length <= 5) 
        return arr
    // First we push the biimplication to the back of the array
    arr.push(getSecondBiimplication(arr))
    // Then we replace the main proposal with the variable
    arr[0] = arr[0].replace(regex, getNewVarName(arr, false))
    // No need to change anything, just passing the same array
    Tseytin(arr)
    return arr
}
input = "((p^q)^q)"
console.log("INPUT - ",input)
console.log("OUTPUT - ",TseytinTransform(input))


const add = x => y => z => x + y + z;

  add(10)(20)(30);