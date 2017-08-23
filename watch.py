import sys
import time
import logging
from watchdog.observers import Observer
from watchdog.events import LoggingEventHandler

defaultExts = \
  [ 'NoMonomorphismRestriction'
  , 'PatternSynonyms'
  , 'OverloadedStrings'
  , 'OverloadedLists'
  , 'AllowAmbiguousTypes'
  , 'ApplicativeDo'
  , 'Arrows'
  , 'BangPatterns'
  , 'BinaryLiterals'
  , 'ConstraintKinds'
  , 'DataKinds'
  , 'DefaultSignatures'
  , 'DeriveDataTypeable'
  , 'DeriveFoldable'
  , 'DeriveFunctor'
  , 'DeriveGeneric'
  , 'DeriveTraversable'
  , 'DoAndIfThenElse'
  , 'DuplicateRecordFields'
  , 'EmptyDataDecls'
  , 'FlexibleContexts'
  , 'FlexibleInstances'
  , 'FunctionalDependencies'
  , 'GeneralizedNewtypeDeriving'
  , 'InstanceSigs'
  , 'LambdaCase'
  , 'MonadComprehensions'
  , 'MultiWayIf'
  , 'NamedWildCards'
  , 'NegativeLiterals'
  , 'NoImplicitPrelude'
  , 'NumDecimals'
  , 'OverloadedLabels'
  , 'PackageImports'
  , 'QuasiQuotes'
  , 'RankNTypes'
  , 'RecursiveDo'
  , 'ScopedTypeVariables'
  , 'StandaloneDeriving'
  , 'TemplateHaskell'
  , 'TupleSections'
  , 'TypeApplications'
  , 'TypeFamilies'
  , 'TypeFamilyDependencies'
  , 'TypeOperators'
  , 'ViewPatterns'
  , 'LiberalTypeSynonyms'
  , 'RelaxedPolyRec'
  ]

class Handler:
    def dispatch (h, evnt):
        print(":r")
        print("main")
        sys.stdout.flush()

if __name__ == "__main__":
    for ext in defaultExts:
        print(":set -X" + ext)
    print(":!clear")
    print(":load Main.hs")
    print("main")
    sys.stdout.flush()

    event_handler = Handler()
    observer = Observer()
    observer.schedule(event_handler, ".", recursive=True)
    observer.start()
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        observer.stop()
    observer.join()
