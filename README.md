Hoare論理に基づいた証明系：z3用パーサと変換器を実装する

**構文
変数宣言，最弱前提条件，プログラム，最強事後条件の順に記述する．

*宣言
    vars x xs[]
最弱事前条件
    Q : x >= 0 & forall y < x
プログラム
    代入
    x = 3, x[i] = k
    比較
    x == y (<=. < . >=, >, not(!), and, or)
    分岐
    if ~ then ~ else ~
    繰り返し
    while ~ inv ~ : ~
