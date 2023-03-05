import fs from 'fs/promises';
import path from 'path';

const lambda = [0.012716, 0.031738, 0.115525, 0.310828, 1.397474, 3.872331];
const beta = [0.038, 0.213, 0.188, 0.407, 0.128, 0.026];
const initial_power = 50;
const l = 0.085; // TODO: check this

function timestampToSeconds(timestamp: string): number {
  const split = timestamp
    .substring(0, timestamp.length - 2)
    .split(':')
    .map((x) => Number(x));
  return split[0] * 60 * 60 + split[1] * 60 + split[2];
}

function sum(list: number[]): number {
  let s = 0;
  list.forEach((n) => {
    s += n;
  });
  return s;
}

function average(list: number[]): number {
  return sum(list) / list.length;
}

function inhour(DT: number): number {
  const stable_period = DT / Math.log(2);
  return (
    (lambda_eff * l + beta_eff + l) /
    (lambda_eff * stable_period + stable_period + 1)
  );
}

const lambda_eff = sum(lambda);
console.log(`Lambad effective: ${lambda_eff}`);
const beta_eff = sum(beta);
console.log(`Beta effective: ${beta_eff}`);

async function main() {
  const rows = (
    await fs.readFile(path.resolve(__dirname, '..', 'input', 'lab3_s1_15s.csv'))
  )
    .toString()
    .split('\n');
  const cicLinear: number[] = [];
  const cicLog: number[] = [];
  const corrected: number[] = [];
  let lastPower = Number(rows[1][6]);
  let lastPowerTime;
  let doublingTimes: number[] = [];
  let outputCsv = 'Linear Power,Log Power,Corrected Power\n';
  let reactivityCsv = `Power,Reactivity\n`;
  for (let i = 1; i < rows.length; i += 1) {
    const row = rows[i].split(',');
    const time = timestampToSeconds(row[0]);
    if (i == 1) {
      lastPowerTime = time;
    }
    const linear = Number(row[3]);
    const log = Number(row[6]);
    cicLinear.push(linear);
    cicLog.push(log);
    const DT = time - (lastPowerTime as number);
    if (i < 115) {
      corrected.push(log);
      if (log >= lastPower * 2) {
        doublingTimes.push(DT);
        lastPower = log;
        lastPowerTime = time;
        reactivityCsv += `${lastPower},${inhour(DT)}\n`;
      }
    } else {
      corrected.push(linear);
      if (linear >= lastPower * 2) {
        doublingTimes.push(DT);
        lastPower = linear;
        lastPowerTime = time;
        reactivityCsv += `${lastPower},${inhour(DT)}\n`;
      }
    }
    outputCsv += `${linear},${log},${corrected[i - 1]}\n`;
  }
  const DT = average(doublingTimes);
  // Should doubling time be the average? Or the mode? Or something else?
  console.log(`DT = ${DT}`);
  const stable_period = DT / Math.log(2);
  console.log(`Stable period = ${stable_period}`);
  const reactivity = inhour(DT);
  console.log(`Reactivity: ${reactivity}`);
  await fs.writeFile(
    path.resolve(__dirname, '..', 'output', 'lab3_s1_15s.csv'),
    outputCsv
  );
  await fs.writeFile(
    path.resolve(__dirname, '..', 'output', 'lab3_s1_15s_reactivity.csv'),
    reactivityCsv
  );
}
main();
