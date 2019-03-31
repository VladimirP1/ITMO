* __Топология__
    * <span style="color:olive;">__топологическое пространство__</span>
        --- упорядоченная пара $\langle X, \Omega\rangle$, 
        где $X$(_носитель_) --- множество, 
        а $\Omega$(_топология_) --- множество каких-то его подмножеств.
    * <span style="color:olive;">__открытое и замкнутое множество__</span>  
        _Открытое множество_ --- множество из $\Omega$.  
        _Замкнутое множество_ --- мн-во, дополнение которого открыто.
    * <span style="color:olive;">__внутренность и замыкание множества__</span>  
        _внутренность $A$_ --- максимальное открытое множество $A^\circ$, входящее в $A$  
        _замыкане $A$_ --- минимальное замкнутое множество $\overline A$, содержащее $A$
    * <span style="color:olive;">__топология стрелки__</span>
        $X=\mathbb R, \Omega = \\{(x,+\infty)|x\in \mathbb R\\} \cup \\{\emptyset\\}$
    * <span style="color:olive;">__дискретная топология__</span>
        $X \neq \emptyset, \Omega = 2\^X $
    * __топология на частично упорядоченном множестве__
    * __индуцированная топология на подпространстве__
    * __связность__

* __Исчисление высказываний__
    * <span style="color:olive;">__высказывание__</span> --- Строка в некотором алфавите, строящаяся по ледующим правилам:
        <pre>
        высказывание :=
            {пропозициональная переменная}  |
            (высказывание |  высказывание)  |
            (высказывание &  высказывание)  |
            (высказывание -> высказывание)
        </pre>  
        A,B,C ... --- _пропозициональные переменные_  
        X,Y,Z ... --- _метапеременные для переменных_
    * __метапеременные__
    * __пропозициональные переменные__
    * <span style="color:olive;">__аксиома__</span> --- высказывание
    * <span style="color:olive;">__схема аксиом__</span> --- шаблон для генерации аксиом
    * <span style="color:olive;">__правило Modus Ponens__</span> --- Если доказано $\alpha$ и $\alpha \rightarrow \beta$, то считаем доказанным $\beta$
    * <span style="color:olive;">__доказательство__</span> --- последовательность высказываний, каждое из которых либо
        аксиома, либо Modus Ponens.
    * <span style="color:olive;">__вывод из гипотез__</span>  
        $\alpha$ выводимо из $\Gamma$, где $\Gamma$ --- список высказываний, если существует _вывод_,
        то есть последовательность высказываний такая, 
        что каждое из них либо аксиома, либо из $\Gamma$, либо получается по M. P.
    * <span style="color:olive;">__доказуемость ($\vdash$)__</span>  
        Высказывание $\alpha$ _доказуемо_, если существует доказательство $\alpha_1 \ldots \alpha_k$,
        где $\alpha_k = \alpha$.
    * __множество истинностных значений__
    * __модель (оценка переменных)__
    * <span style="color:olive;">__оценка высказывания__</span> --- Отображение: формула $\rightarrow$ множество истинностных значений
    * <span style="color:olive;">__общезначимость ($\models$)__</span> --- истинность при любой оценке
    * __выполнимость__
    * __невыполнимость__
    * __следование__
    * <span style="color:olive;">__корректность__</span> --- доказуемость $\implies$ общезначимость
    * <span style="color:olive;">__полнота__</span> --- общезначимость $\implies$ доказуемость
    * __противоречивость__
    * <span style="color:olive;">__формулировка теоремы о дедукции__</span> $\Gamma \vdash \alpha \rightarrow \beta \iff \Gamma, \alpha \vdash \beta$

* __Интуиционистское исчисление высказываний__
    (заменили аксиому снятия двойного отрицания на $\alpha \rightarrow \neg \alpha \rightarrow \beta$)
    * <span style="color:olive;">__закон исключённого третьего__</span> $\alpha \vee \neg \alpha$
    * <span style="color:olive;">__закон снятия двойного отрицания__</span> $\neg \neg \alpha \rightarrow \alpha$
    * <span style="color:olive;">__закон Пирса__</span> $((\alpha \rightarrow \beta) \rightarrow \alpha) \rightarrow \alpha$
    * <span style="color:olive;">__Все эти законы не выводятся в ИИВ__</span>
    * <span style="color:olive;">__BHK-интерпретация логических связок__</span>  
        $\alpha \\& \beta$, если есть доказательство $\alpha$ и $\beta$  
        $\alpha \vee \beta$, если есть доказательство $\alpha$ или $\beta$ и мы знаем, чего именно  
        $\alpha \rightarrow \beta$, если мы умеем строить доказательство $\beta$ из доказательства $\alpha$  
        $\neg \alpha$, если из $\alpha$ можно построить противоречие ($\alpha \rightarrow \perp$)
    * <span style="color:olive;">__теорема Гливенко (формулировка)__</span> Если $\vdash\_\{\text\{к\}\} \alpha$, то $\vdash\_\{\text\{и\}\} \neg \neg \alpha$
    * <span style="color:olive;">__решётка__</span>  
        $\langle A, \leq \rangle$ --- решётка, если:  
        * $\forall a,b \in A: \quad \exists \text{наименьший } c = a + b : a \leq c, b \leq c $
        * $\forall a,b \in A: \quad \exists \text{наибольший } c = a \cdot b : c \leq a, c \leq b $
    * <span style="color:olive;">__дистрибутивная решётка__</span>  
        решётка + свойство: $a+(b \cdot c) = a \cdot b + a \cdot c$  
        __лемма__: $a\cdot(b+c) = a\cdot b + a \cdot c$  
        __теорема__: решётка дистрибутивна $\iff$ не содержит ни диаманта ни пентагона
    * <span style="color:olive;">__импликативная решётка__</span>  
        дистрибутивная решётка + определена операция псевдодополнения (относительно $b$):
            $c = a \rightarrow b = max \\{ x | x \cdot a \leq b \\}$  
        __теорема__: дистрибутивность в определении можно опустить  
        __def__: _1_ --- наибольший элемент решётки  
        __def__: _0_ --- наименьший элемент решётки
    * <span style="color:olive;">__алгебра Гейтинга__</span> --- Импликативная решётка с 0  
        __def__: _псевдодополнение_ $~a = a \rightarrow 0$
    * <span style="color:olive;">__булева алгебра__</span> --- алгебрa Гейтинга такая, что $\forall a: a + ~a = 1$
    * __гомоморфизм алгебр Гейтинга__
    * <span style="color:olive;">__Гёделева алгебра__</span>  
        Алгебра Гейтинга _гёделева_, если $\forall a,b : (a+b=1 \implies a=1 | b=1)$
    * <span style="color:olive;">__операция Γ(A)__</span>
        Добавим к алгебре Гейтинга новую "1", большую всех элементов, а старую переименуем в "$\omega$".
    * <span style="color:olive;">__алгебра Линденбаума__</span>
        Пусть $\alpha$, $\beta$ --- формулы, $\alpha \leq \beta$, если $\beta\vdash\alpha$,
        $\alpha \approx \beta$, если $\alpha \leq \beta \\& \beta \leq \alpha $  
        Тогда, _алгебра Линденбаума_ --- $\text{ИИВ}/\_\{\approx\}$ [факторизация по операции $\approx$]
    * <span style="color:olive;">__формулировка свойства дизъюнктивности и.и.в__</span> --- $\vdash \alpha \vee \beta \implies \vdash \alpha \text{ или } \vdash \beta$
    * __формулировка свойства нетабличности и.и.в.__

* __Исчисление предикатов__
    * <span style="color:green;">__предикатные и функциональные символы, константы и пропозициональные переменные__</span>
    * <span style="color:green;">__свободные и связанные вхождения предметных переменных в формулу__</span>
    * <span style="color:green;">__свобода для подстановки__</span>
        <pre>
        <span style="color:red;">D</span> _предметное множество_
        <span style="color:green;">V</span> _множество истинностынх значений_  
        <span style="color:red;">_ФУНКЦИЯ_</span>  : D^n -> D
        <span style="color:green;">_ПРЕДИКАТ_</span> : D^n -> V  
        <span style="color:red;">_Предметная переменная_</span>      a, b, c, x, y, z, a₀, a' ...
        <span style="color:red;">_Терм_</span>                       θ₀, θ₁ ...
        <span style="color:green;">_Предикатный символ_</span>         P
        <span style="color:green;">_Формула_</span>                    α, ψ, φ  
        <span style="color:red;">ТЕРМ</span> =
                (предметная переменная) |
                (функциональный символ) (ТЕРМ₀, ТЕРМ₁, ...)
        <span style="color:green;">ФОРМУЛА =
                (ФОРМУЛА |  ФОРМУЛА) |
                (ФОРМУЛА &  ФОРМУЛА) |
                (ФОРМУЛА -> ФОРМУЛА) |
                (!ФОРМУЛА) |
                (∀ <span style="color:red;">предметная переменная</span>.ФОРМУЛА) |
                (∃<span style="color:red;"> предметная переменная</span>.ФОРМУЛА) |</span>
                (<span style="color:green;">предикатный символ</span>)(<span style="color:red;">ТЕРМ</span>₀,<span style="color:red;"> ТЕРМ</span>₁, ...)  
        _Связанное_ вхождение --- вхождение в области действия квантора.
        _Связывающее_ вхождение --- вхождение непосредственно рядом с квантором.
        Ex: (∀x. … x …) первое вхождение --- связывающее, второе вхождение --- связанное.
        Не связанные и не связывающие вхождения --- _свободные_.
        Терм θ _свободен для подстановки_ в формулу ψ вместо x, если после подстановки θ вместо свободных вхождений x, θ не станет связанным.
        </pre>
    * <span style="color:green;">__два правила для кванторов__</span>  
        $\newenvironment{rcases}{\left.\begin{array}}{\end{array}\right\rbrace}$
        \\(\begin{rcases}{l}11. (\forall x.\phi) \rightarrow \phi [x:=\Theta]\\\\12. \phi [x:=\Theta] \rightarrow \exists x.\phi\end{rcases}\\)
        , где $\Theta$ свободна для подстановки вместо $x$ в $\phi$
    * <span style="color:green;">__две аксиомы для кванторов__</span>  
        \\(\begin{rcases}{l}2. \dfrac{\psi \rightarrow \phi}{\psi \rightarrow \forall x.\phi} \\\\3. \dfrac{\phi \rightarrow \psi}{(\exists x.\phi) \rightarrow \psi} \end{rcases}\\)
        , где $x$ не входит свободно в $\psi$
    * __оценки и модели в исчислении предикатов__
    * __теорема о дедукции для И. П.__
    * __теорема о корректности для И. П.__
    * __полное множество (бескванторных) формул__
    * __модель для формулы__
    * __теорема Гёделя о полноте исчисления предикатов (формулировка)__
    * __следствие из теоремы Гёделя о исчислении предикатов__


[Шень, Верещагин](https://www.mccme.ru/free-books/shen/shen-logic-part2-2.pdf)
[Инт. логика](http://lpcs.math.msu.su/~plisko/intlog.pdf)
[Конспект 2011](https://github.com/shd/logic2011/blob/master/conspect.pdf)
