var colors = [ '#ff00ff', '#ff00e6', '#ff00cc', '#ff00b2', '#ff0099',
    '#ff0080', '#ff0066', '#ff004c', '#ff0033', '#ff001a', '#ff0000', '#ff1a00',
    '#ff3300', '#ff4c00', '#ff6600', '#ff8000', '#ff9900', '#ffb200', '#ffcc00',
    '#ffe600', '#ffff00', '#e6ff00', '#ccff00', '#b2ff00', '#99ff00', '#80ff00',
    '#66ff00', '#4cff00', '#33ff00', '#1aff00', '#00ff00', '#00ff1a', '#00ff33',
    '#00ff4c', '#00ff66', '#00ff80', '#00ff99', '#00ffb2', '#00ffcc', '#00ffe6',
    '#00ffff', '#00e6ff', '#00ccff', '#00b2ff', '#0099ff', '#0080ff', '#0066ff',
    '#004cff', '#0033ff', '#001aff', '#0000ff', '#0909fb', '#1212f7', '#1a1af3',
    '#2323ef', '#2c2ceb', '#3535e7', '#3e3ee3', '#4646df', '#4f4fdb', '#5858d8',
    '#6161d4', '#6a6ad0', '#7272cc', '#7b7bc8', '#8484c4', '#8d8dc0', '#9696bc',
    '#9e9eb8', '#a7a7b4', '#b0b0b0', '#a7a7a7', '#9f9f9f', '#969696', '#8d8d8d',
    '#848484', '#7c7c7c', '#737373', '#6a6a6a', '#616161', '#595959', '#505050', ];

const SPECIAL = 0;
const REGULAR = 10;
const MAX_COLOR = colors.length - 1;
var idx = MAX_COLOR;
var primeCounter = [-100, -100, -100, -100];


// Compute prime factors
function factors(n) {
    var top = Math.ceil(Math.sqrt(n));
    var factors = [];

    while (n % 2 === 0) {
        factors.push(2);
        n /= 2;
    }

    for (let o = 3; o <= top; o += 2) {
        while (n % o === 0) {
            factors.push(o);
            n /= o;
        }
    }

    if (n !== 1)
        factors.push(n);

    return factors;
}


function recognizer(counter) {
    //console.log(counter); // DELETE ME
    if ((counter[0] == 0 && counter[1] == 3 && counter[2] == 5) ||
        (counter[0] == 0 && counter[1] == 1 && counter[2] == 5)) {
        return 'triplet';
    }
    else if (counter[0] == 0 && counter[1] == 7) {
        return 'octomus';
    }
    else if (counter[0] == 0 && counter[1] == 5) {
        return 'sexy';
    }
    else if (counter[0] == 0 && counter[1] == 3) {
        return 'cousin';
    }
    else if (counter[0] == 0 && counter[1] == 1) {
        return 'twin';
    }
    else if (counter[1] > 59) {
        return 'combo-breaker';
    }
    else {
        return false;
    }
}


((print, the, banner) => {
    let i = SPECIAL;
    for (let line of [
        "## *** ***** ******* *********** ************* *****************",
        "** *** ***** ******* *  IT'S PRIME TIME!  **** *****************",
        "** *** ***** ******* *   By Erik Falor    **** *****************",
        "** *** *****  https://github.com/fadein/primetime **************",
        "** *** ***** ******* * Copyright (c) 2020 **** *****************",
        "** *** ***** ******* *********** ************* #################"]) {
        var pre = document.createElement('pre');
        var span =  document.createElement('span');
        span.textContent = line
        span.style = `color: black; background-color: ${colors[i++]}`;
        pre.appendChild(span);
        document.querySelector('#console').appendChild(pre);
    }
})();


var tick = (() => {
    var begin = Math.trunc((new Date()).getTime() / 1000);
    var now = begin;
    var cons = document.querySelector('#console');
    return () => {
        var f = factors(now);
        var pre = document.createElement('pre');
        if (f.length == 1) {
            primeCounter.pop();
            primeCounter.unshift(0);
            switch (recognizer(primeCounter)) {
                case 'quadruple':
                    pre.textContent = `${now}: ** *** PRIME ******* QUADRUPLET! ************* *****************`;
                    idx = SPECIAL;
                    break;

                case 'triplet':
                    pre.textContent = `${now}: ** *** PRIME TRIPLET *********** ************* *****************`;
                    idx = SPECIAL;
                    break;

                case 'octomus':
                    pre.textContent = `${now}: ** *** ***** OCTOMUS ***PRIME***`;
                    idx = SPECIAL;
                    break;

                case 'sexy':
                    pre.textContent = `${now}: ** *** *SEXY PRIME**`;
                    idx = SPECIAL;
                    break;

                case 'cousin':
                    pre.textContent = `${now}: ** *** PRIME COUSIN* ***********`;
                    idx = SPECIAL;
                    break;

                case 'twin':
                    pre.textContent = `${now}: ** *** *TWIN PRIME** *********** *************`;
                    idx = SPECIAL;
                    break;

                case 'combo-breaker':
                    let gap = primeCounter[1] + 1;
                    pre.textContent = `${now}: ** CCC COMBO BREAKER *PRIME*GAP=${gap}`;
                    idx = REGULAR;
                    break;

                default:
                    pre.textContent = `${now}: ** *** PRIME TIME***`;
                    idx = REGULAR;
            }
        }
        else {
            // Unrolled loop
            primeCounter[0]++;
            primeCounter[1]++;
            primeCounter[2]++;
            primeCounter[3]++;
            pre.textContent = `${now}: ${f.join(' ')}`;
        }

        var color = colors[idx];
        if (idx < MAX_COLOR)
            idx++;

        pre.style = `color: ${color}`;
        cons.appendChild(pre);
        pre.scrollIntoView();

        // Remove old lines from top of console to save memory
        if (now++ - begin > 257)
            cons.firstElementChild.remove();
    };
})();


setInterval(tick, 1000);
