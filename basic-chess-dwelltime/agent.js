// Config: board coordinates — example 3x3 (change to 8x8 if needed)
const rows = ['3', '2', '1'];  // rank (row) bottom to top in chess notation
const cols = ['a', 'b', 'c'];  // file (column) left to right

const dwellTimes = {};
let currentSquare = null;
let enterTimestamp = null;

function onSquareEnter(squareId) {
  if (currentSquare !== null) {
    // Calculate dwell time for previous square
    const now = Date.now();
    const delta = now - enterTimestamp;
    dwellTimes[currentSquare] = (dwellTimes[currentSquare] || 0) + delta;
  }
  currentSquare = squareId;
  enterTimestamp = Date.now();
}

function onSquareLeave() {
  if (currentSquare !== null) {
    const now = Date.now();
    const delta = now - enterTimestamp;
    dwellTimes[currentSquare] = (dwellTimes[currentSquare] || 0) + delta;
    currentSquare = null;
    enterTimestamp = null;
  }
}

// Attach event listeners to squares, assuming each square has class 'square' and id like 'a1', 'b3', etc.
document.querySelectorAll('.square').forEach(square => {
  square.addEventListener('mouseenter', () => onSquareEnter(square.id));
  square.addEventListener('mouseleave', () => onSquareLeave());
});

// Every 5 seconds, log dwell time in seconds, rounded
setInterval(() => {
  // Also close out dwell time if mouse is currently inside a square to keep counts accurate
  if (currentSquare !== null && enterTimestamp !== null) {
    const now = Date.now();
    const delta = now - enterTimestamp;
    dwellTimes[currentSquare] = (dwellTimes[currentSquare] || 0) + delta;
    enterTimestamp = now;
  }

  const dwellSeconds = {};
  for (const sq in dwellTimes) {
    dwellSeconds[sq] = +(dwellTimes[sq] / 1000).toFixed(2);
  }

  console.clear();
  console.log('Dwell time per square (seconds):', dwellSeconds);
}, 5000);
