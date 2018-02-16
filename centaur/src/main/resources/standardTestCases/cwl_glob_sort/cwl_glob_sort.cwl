cwlVersion: v1.0
$graph:
- id: globSort
  cwlVersion: v1.0
  class: CommandLineTool
  requirements:
    - class: InlineJavascriptRequirement

  inputs: []
  baseCommand: [touch, z, y, x, w, c, b, a]
  outputs:
    letters:
      type: File[]
      outputBinding:
        glob: '*'
        outputEval: |
          ${ return self.sort(function(a,b) { return a.location > b.location ? 1 : (a.location < b.location ? -1 : 0) }) }