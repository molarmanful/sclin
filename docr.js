import { readFile, writeFile } from 'fs'
import { exec } from 'child_process'
import plim from 'p-limit'

let lim = plim(5)
let replaceAsync = async (s, r, f) => {
  let all = await Promise.all(Array.from(s.matchAll(r), a => lim(_ => f(...a))))
  let i = 0
  return s.replace(r, _ => all[i++])
}

let fl = process.argv[2]
readFile(fl, (e, data) => {
  if (e) throw e
  else {
    replaceAsync(data + '', /```sclin\s*?\n([^]+?)\n```/g, (_, x) => {
      console.log('EXEC:', x)
      return new Promise(resolve => {
        exec(
          `out/sclin/assembly.dest/out.jar --doceval '${x.replace(
            /'/g,
            `'\\''`
          )}'`,
          (e, res) => {
            if (e) throw e
            else {
              let ex = res.trim().split`\n`
              let e = ex.pop()
              let o =
                '```\n' +
                `${x}\n${e}${ex.length ? '\n' + ex.join`\n` : ''}` +
                '\n```'
              console.log('DONE:', o)
              resolve(o)
            }
          }
        )
      })
    }).then(res => {
      writeFile(fl, res, _ => {})
    })
  }
})
