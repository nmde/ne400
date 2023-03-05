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
  const period = DT / Math.log(2);
  return (lambda_eff * l + beta_eff + l) / (lambda_eff * period + period + 1);
}

const lambda_eff = sum(lambda);
const beta_eff = sum(beta);

class DataRow {
  public time: number;
  public power: number;
  public DT!: number;
  public power_coefficient!: number;
  public power_defect!: number;

  public constructor(time: number, power: number) {
    this.time = time;
    this.power = power;
  }

  public get reactivity(): number {
    return inhour(this.DT);
  }
}

async function main(experiment: string) {
  const input = (
    await fs.readFile(
      path.resolve(__dirname, '..', 'input', `lab3_s1_${experiment}.csv`)
    )
  )
    .toString()
    .split('\n');
  let outputCsv =
    'Time,Corrected Power,Doubling Time,Reactivity,Power Coefficient,Power Defect\n';
  const rows: DataRow[] = [];
  for (let i = 1; i < input.length - 1; i += 1) {
    const data = input[i].split(',');
    const time = timestampToSeconds(data[0]);
    let row: DataRow;
    if (
      (i < 368 && experiment === '40s') ||
      (i < 115 && experiment === '15s')
    ) {
      row = new DataRow(time, Number(data[6]));
    } else {
      row = new DataRow(time, Number(data[3]));
    }
    rows.push(row);
  }
  for (let i = 0; i < rows.length; i += 1) {
    let found = false;
    let j = 0;
    while (!found && j < rows.length) {
      if (rows[j].power > rows[i].power / 2) {
        found = true;
      } else {
        j += 1;
      }
    }
    if (found) {
      rows[i].DT = rows[i].time - rows[j].time;
      console.log(`${rows[i].time} - ${rows[j].time}`);
      if (i > 0) {
        const delta_rho = rows[i].reactivity - rows[i - 1].reactivity;
        const delta_P = rows[i].power - rows[i - 1].power;
        if (rows[i].power === rows[i - 1].power) {
          rows[i].power_coefficient = rows[i - 1].power_coefficient;
          rows[i].power_defect = rows[i - 1].power_defect;
        } else {
          rows[i].power_coefficient = delta_rho / delta_P;
          rows[i].power_defect = rows[i].power_coefficient * delta_P;
        }
      }
    } else {
      console.warn(`Could not calculate for power ${rows[i].power}!`);
    }
    outputCsv += `${rows[i].time},${rows[i].power},${rows[i].DT},${rows[i].reactivity},${rows[i].power_coefficient},${rows[i].power_defect}\n`;
  }
  const DT = average(rows.map((row) => row.DT));
  console.log(`DT = ${DT}`);
  const stable_period = DT / Math.log(2);
  console.log(`Stable period = ${stable_period}`);
  const reactivity = inhour(DT);
  console.log(`Reactivity: ${reactivity}`);
  const power_coefficient_total =
    (rows[rows.length - 1].reactivity - rows[0].reactivity) /
    (rows[rows.length - 1].power - rows[0].power);
  console.log(`Estimated total power coefficient: ${power_coefficient_total}`);
  const power_defect_total =
    power_coefficient_total * (rows[rows.length - 1].power - rows[0].power);
  console.log(`Estimated total power defect: ${power_defect_total}`);
  await fs.writeFile(
    path.resolve(__dirname, '..', 'output', `lab3_s1_${experiment}.csv`),
    outputCsv
  );
}

main('15s');
main('40s');
