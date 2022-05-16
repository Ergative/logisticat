#!/usr/bin/env python3

"""Script to create single .R source file from the whole package.

For anyone that doesn't want to download the whole devtools package - which,
at least in a fresh R installation, can take a long time - a single .R file
implementing the same functionality might be more convenient.

This script gathers up most of the .R files in the <package root>/R directory,
concatenates them, then prepends some library() calls to the start to make up
for things that would have gotten made available to the package without
using the :: operator and thus would otherwise require code changes.

"""

from datetime import date
from os import listdir
from os.path import isfile, join

def comment(line: str) -> str:
  """Prefix a # to make a Python comment."""
  return "# " + line

def make_header(name: str, descriptionFile: str, licenseFile:str) -> str:
  desc = ""
  lic = ""
  with open(descriptionFile, "r") as f:
    desc = "".join([comment(line) for line in f])
  with open(licenseFile, "r") as f:
    lic = "".join([comment(line) for line in f])
    
  advice = comment("NOTE: You will need to install the following packages:\n") +\
    "\n".join([comment(line) for line in parse_package_dependencies(descriptionFile)]) +\
    "\n"
    
  return "\n".join([comment(name) + "\n", lic, comment("This source file was automatically" +\
                    " generated from an R project on " +\
                    date.today().strftime("%Y/%m/%d") + ".\n"), advice, desc]) + "\n\n"

def get_all_r_files(directory: str) -> list:
  """Returns a list of all .R files in a directory."""
  return [f for f in listdir(directory) if isfile(join(directory, f)) and f.endswith(".R")]

def parse_package_dependencies(description_file: str) -> list:
  """Return a list of packages that this file will depend on.
  
  This is just everything listed under Imports in the DESCRIPTION file.
  """
  with open(description_file, "r") as f:
    contents = f.read()
    return [s.strip() for s in contents.split("Imports:")[1].split("Suggests:")[0].split(",")]

def parse_importFrom(namespace_file: str) -> list:
  """Return [namespace, symbol] pairs from NAMESPACE file.
  
  These are parsed from all lines of the form:
    importFrom(otherNamespace, symbolToImport)
    
  For example:
    importFrom(magrittr,"%>%")
    
  The above will yield an element in the returned list like:
    ['magrittr', '"%>%'"]
  
  Parameters
  ----------
  namespace_file : str
    The package's NAMESPACE file.
    
  Returns
  -------
  list
    A list of lists, one per importFrom statement, where the first element of
    each sub-list is the namespace from which the symbol is to be imported and
    the second element of each sub-list is the symbol (e.g., name of a function)
    that needs to be imported from that namespace.
  """
  with open(namespace_file, "r") as f:
    return [line.split("(")[1].split(")")[0].split(",") for line in f if line.startswith("importFrom")]
  
  
def read_file(file: str) -> str:
  """Read the contents of a file into a string."""
  with open(file, "r") as f:
    return f.read()

def concatenate_file_contents(files: list) -> str:
  """Concatenate contents of all listed files into one string."""
  return "\n".join([read_file(f) for f in files])

def emit_file_name_comment(fileName: str) -> str:
  """Make a comment line naming the file."""
  return ("## ==== " + fileName + " ").ljust(77, "=") + " ##\n\n"

def emit_symbol_load(namespace: str, symbol: str) -> str:
  """Create a line of R code loading the symbol from the specified namespace."""
  return symbol + " <- " + namespace + "::" + symbol

def build(name, descriptionFile, licenseFile, rDirectory, namespaceFile, exludeList):
  header = make_header(name, descriptionFile, licenseFile)
  imports = parse_importFrom(namespaceFile)
  importCode = emit_file_name_comment("IMPORTED SYMBOLS") +\
    "\n".join([emit_symbol_load(pair[0], pair[1]) for pair in imports]) + "\n\n"
  
  exclude = []
  with open(exludeList, "r") as f:
    exclude = [line.strip() for line in f]

  rFiles = get_all_r_files(rDirectory)
  rFiles = [f for f in rFiles if not f in exclude]
  
  rCode = "\n".join([emit_file_name_comment(f) + read_file(join(rDirectory,f)) for f in rFiles])
  return "\n".join([header, importCode, rCode])

def mkdir_if_not_exist(directory: str):
  """Create a directory without complaining if it already exists."""
  try:
    os.mkdir(directory)
  except FileExistsError:
    pass # OK
  
def parseBuildInfo(path: str) -> dict:
  d = {}
  with open(path, "r") as f:
    for line in f:
      kv = line.split(":", 1)
      if (len(kv) == 2):
        d[kv[0].strip()] = kv[1].strip()
  return d

if __name__ == "__main__":
  info = parseBuildInfo("scripts/build_info.txt")
  name = info["name"]
  descriptionFile = info["descriptionFile"]
  licenseFile = info["licenseFile"]
  rDirectory = info["rDirectory"]
  namespaceFile = info["namespaceFile"]
  excludeList = info["excludeList"]
  outFile = info["outFile"]
  outDir = info["outDir"]
  
  mkdir_if_not_exist(outDir)
  
  with open(join(outDir,outFile), "w") as f:
    f.write(build(name, descriptionFile, licenseFile,
      rDirectory, namespaceFile, excludeList))
  
  
  
  
 
  
  
  
  
  
