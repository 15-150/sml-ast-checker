
structure BasicClassifier = MkClassifier (open Classification)
structure EmptyClassifier =
  MkClassifier (
    open Classification
    val init_classifications = []
  )
