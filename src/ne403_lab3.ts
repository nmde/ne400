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
  const reactivities: number[] = [];
  const powers: number[] = [];
  let lastPower = Number(rows[1][6]);
  let lastPowerTime: number | null;
  let doublingTimes: number[] = [];
  let outputCsv = 'Linear Power,Log Power,Corrected Power\n';
  let reactivityCsv = `Power,Reactivity,Power Coefficient,Power Defect\n`;

  const processRow = (power: number, time: number) => {
    const DT = time - (lastPowerTime as number);
    corrected.push(power);
    if (power >= lastPower * 2) {
      doublingTimes.push(DT);
      lastPower = power;
      powers.push(power);
      lastPowerTime = time;
      reactivities.push(inhour(DT));
      let power_coefficient = 0;
      let power_defect = 0;
      if (reactivities.length > 1) {
        const delta_rho =
          reactivities[reactivities.length - 1] -
          reactivities[reactivities.length - 2];
        const delta_P = powers[powers.length - 1] - powers[powers.length - 2];
        power_coefficient = delta_rho / delta_P;
        power_defect = power_coefficient * delta_P;
      }
      reactivityCsv += `${lastPower},${
        reactivities[reactivities.length - 1]
      },${power_coefficient},${power_defect}\n`;
    }
  };

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
    if (i < 115) {
      processRow(log, time);
    } else {
      processRow(linear, time);
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
  // Is this a good estimate?
  const power_coefficient_total =
    (reactivities[reactivities.length - 1] - reactivities[0]) /
    (powers[powers.length - 1] - powers[0]);
  console.log(`Estimated total power coefficient: ${power_coefficient_total}`);
  const power_defect_total = power_coefficient_total * (powers[powers.length - 1] - powers[0]);
  console.log(`Estimated total power defect: ${power_defect_total}`);
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
