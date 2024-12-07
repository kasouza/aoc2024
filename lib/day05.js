// YEAAH JS CAUSE KIND OF IN A RUSH 
// CAMEL BAD JAVAS GOOD

const rules = [
  // 57|24
  [57, 24],
];
const allPages = [
  // 13, 19, 52, 14, 65, 96, 62, 98, 58, 73, 97, 92, 91, 87, 44, 54, 42
  [13, 19, 52, 14, 65, 96, 62, 98, 58, 73, 97, 92, 91, 87, 44, 54, 42],
]

console.log({
  part1: part1(),
  part2: part2(),
})

function part1() {
  let answer = 0;
  for (const pages of allPages) {
    const [ok] = isPagesValid(pages);
    if (ok) {
      const middleIndex = Math.floor(pages.length / 2);
      answer += pages[middleIndex];
    }
  }

  return answer;
}

function part2() {
  let answer = 0;
  for (const pages of allPages) {
    let [ok, brokenRule] = isPagesValid(pages);
    if (ok) {
      continue;
    }

    do {
      const [fromIdx, toIdx] = brokenRule;

      const temp = pages[fromIdx];

      pages[fromIdx] = pages[toIdx];
      pages[toIdx] = temp;

      [ok, brokenRule] = isPagesValid(pages);
    } while(!ok);

    const middleIndex = Math.floor(pages.length / 2);
    answer += pages[middleIndex];
  }

  return answer;
}

function isPagesValid(pages) {
  for (const page of pages) {
    const otherPages = pages.filter(otherPage => page !== otherPage);
    const rules = findRules(page, otherPages);

    if (!rules.length) {
      continue;
    }

    const [ok, brokenRule] = checkRules(pages, rules);
    if (!ok) {
      return [false, brokenRule];
    }
  }

  return [true, null];
}


function findRules(page, otherPages) {
  return rules.filter(([from, to]) => from === page && otherPages.includes(to));
}

/**
 * @param {number[]} pages
 * @param {number[][]} rules
 */
function checkRules(pages, rules) {
  for (const [from, to] of rules) {
    const fromIdx = pages.findIndex(page => page === from);
    const toIdx = pages.findIndex(page => page === to);

    if (fromIdx === -1 || toIdx === -1) {
      continue;
    }

    const brokenRule = [fromIdx, toIdx];
    if (fromIdx >= toIdx) {
      return [false, brokenRule];
    }
  }

  return [true,null];
}
