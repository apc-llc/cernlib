*
* $Id: xxreqc.s,v 1.1.1.1 1996/04/01 15:03:22 mclareni Exp $
*
* $Log: xxreqc.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:22  mclareni
* Mathlib gen
*
*
          IDENT  XXREQC
          SPACE  1
          ENTRY  XXREQC
          SPACE  4
TRACE     VFD    42/6LXXREQC,18/XXREQC
SAVEA0     BSSZ   1
          SPACE  1
          USE    /XREQTAG/
XREQTAG   BSSZ   10
          USE    *
          SPACE  1
EXIT      SA1    SAVEA0
          SA0    X1
          SPACE  1
XXREQC   EQ     *+1S17
          SPACE  1
          SX6    A0                SAVE A0
          SA6    SAVEA0
          SPACE  1
          REQUEST XREQTAG
          SPACE  1
          SA2    XREQTAG
          MX0    55
          AX2    9
          BX6    -X0*X2
          SA6    XREQTAG+9
          SPACE  1
          EQ     EXIT
          END
