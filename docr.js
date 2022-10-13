let fs = require('fs')
let cp = require('child_process')

let f = process.argv[2]
fs.readFile(f, (e, data) => {
  if (e) throw e
  else {
    let res = (data + '').replace(/```sclin\s*?\n([^]+?)\n```/g, (_, x) => {
      let ex = (cp.execSync(`./mill sclin --doceval '${x.replace(/'/g, `'\\''`)}'`) + '').trim().split`\n`
      let e = ex.pop()
      let o = '```\n' + `${x}\n${e}${ex.length ? '\n' + ex.join`\n` : ''}` + '\n```'
      console.log(o)
      return o
    })
    fs.writeFile(f, res, _ => { })
  }
})