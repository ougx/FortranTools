ptf $

BEGIN OPTIONS
  PRINT_OPTION                    SUMMARY
  COMPLEXITY                      COMPLEX
  NO_PTC                          FIRST
END OPTIONS

BEGIN NONLINEAR
  OUTER_DVCLOSE                   $OUTER_DVCLOSE      $
  OUTER_MAXIMUM                   $OUTER_MAXIMUM      $
  BACKTRACKING_NUMBER             $BT_NUMBER          $
  BACKTRACKING_TOLERANCE          $BT_TOLERANCE       $
  BACKTRACKING_REDUCTION_FACTOR   $BT_REDUCTION_FACTOR$
  BACKTRACKING_RESIDUAL_LIMIT     $BT_RESIDUAL_LIMIT  $
  UNDER_RELAXATION_THETA          $RELAXATION_THETA   $
  UNDER_RELAXATION_KAPPA          $RELAXATION_KAPPA   $
  UNDER_RELAXATION_GAMMA          $RELAXATION_GAMMA   $
  UNDER_RELAXATION_MOMENTUM       $RELAXATION_MOMENTUM$
END NONLINEAR

BEGIN LINEAR
  INNER_MAXIMUM                   $INNER_MAXIMUM      $         
  INNER_DVCLOSE                   $INNER_DVCLOSE      $         
  INNER_RCLOSE                    $INNER_RCLOSE       $         
  LINEAR_ACCELERATION             $LINEAR_ACCELERATION$         
  RELAXATION_FACTOR               $RELAXATION_FACTOR  $         
  PRECONDITIONER_LEVELS           $LEVELS             $
  PRECONDITIONER_DROP_TOLERANCE   $DROP_TOLERANCE     $
  NUMBER_ORTHOGONALIZATIONS       $ORTHOGONALIZATIONS $  
  SCALING_METHOD                  $SCALING_METHOD     $         
  REORDERING_METHOD               $REORDERING_METHOD  $         
END LINEAR
