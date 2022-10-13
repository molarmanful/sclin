let fs = require('fs')
let cp = require('child_process')

async function replaceAsync(s, r, f) {
  let all = await Promise.all(Array.from(s.matchAll(r), a => f(...a)))
  return s.replace(r, _ => all.shift())
}

let fl = process.argv[2]
fs.readFile(fl, (e, data) => {
  if (e) throw e
  else {
    replaceAsync(data + '', /```sclin\s*?\n([^]+?)\n```/g, (_, x) => new Promise(resolve => {
      cp.exec(`./mill sclin.run --doceval '${x.replace(/'/g, `'\\''`)}'`, (e, res) => {
        if (e) throw e
        else {
          let ex = res.trim().split`\n`
          let e = ex.pop()
          let o = '```\n' + `${x}\n${e}${ex.length ? '\n' + ex.join`\n` : ''}` + '\n```'
          console.log(o)
          resolve(o)
        }
      })
    })).then(res => {
      fs.writeFile(fl, res, _ => { })
    })
  }
})