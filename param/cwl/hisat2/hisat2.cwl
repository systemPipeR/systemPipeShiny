cwlVersion: v1.2
class: CommandLineTool
baseCommand: hisat2
inputs:
  S:
    type: File
    inputBinding:
      prefix: -S
  x:
    type: File
    inputBinding:
      prefix: -x
  k:
    type: int
    inputBinding:
      prefix: -k
  threads:
    type: int
    inputBinding:
      prefix: -threads
  V:
    type: ''
    inputBinding:
      prefix: -V
  min-intronlen:
    type: int
    inputBinding:
      prefix: --min-intronlen
  no_prefix1:
    type: string
    inputBinding:
      prefix: ~
outputs:
  output1:
    type: File
    outputBinding:
      glob: ./results/_SampleName_.sam
