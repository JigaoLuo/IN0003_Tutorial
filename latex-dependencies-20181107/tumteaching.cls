% vi: ft=tex
%
% After several painful hours trying to fix the old template used at our chair,
% I started from scratch resulting in this brand new template.
% I decided to make use of the moving target
%
%      LaTeX3
%
% If this was a good or bad choice, I cannot say so far. Just for the lols,
% if you are still using this template in the future please let me know
% if it still works, or if you had to change something.
% You can contact me: stefan.sf@mytum.de   --   2016-04-01 Stefan SF

\RequirePackage{expl3}

\NeedsTeXFormat{LaTeX2e}

\LoadClass[12pt,a4paper]{article}

\ProvidesExplClass{tumteaching}{0000/00/00}{0.0}{tumteaching}

\RequirePackage{xparse}
\RequirePackage[paper=a4paper,left=25mm,right=25mm,top=20mm,bottom=20mm,marginparwidth=18mm]{geometry}
\RequirePackage{etoolbox}
\RequirePackage{lastpage}
\RequirePackage{graphicx}
\RequirePackage{amssymb}

\bool_new:N \g__tumteaching_german_bool
\bool_new:N \g__tumteaching_is_exam_bool
\bool_new:N \g__tumteaching_show_solution_bool
\bool_new:N \g__tumteaching_show_lab_solution_bool
\bool_new:N \g__tumteaching_show_point_sum_bool
\DeclareOption{german}   { \bool_gset_true:N \g__tumteaching_german_bool }
\DeclareOption{exam}     { \bool_gset_true:N \g__tumteaching_is_exam_bool }
\DeclareOption{solution} { \bool_gset_true:N \g__tumteaching_show_solution_bool }
\DeclareOption{labsolution} { \bool_gset_true:N \g__tumteaching_show_lab_solution_bool }
\DeclareOption{showpointsum} { \bool_gset_true:N \g__tumteaching_show_point_sum_bool }
\ProcessOptions\relax

\int_new:N \g__tumteaching_asn_number_int
\int_new:N \g__tumteaching_asn_total_numbers_int
\fp_new:N \g__tumteaching_asn_points_fp
\fp_new:N \g__tumteaching_asn_total_points_fp % excluding bonus points
\fp_new:N \g__tumteaching_asn_bonus_points_fp
\fp_new:N \g__tumteaching_asn_total_bonus_points_fp
\int_new:N \g__tumteaching_obj_number_int

\fp_new:N \l__tumteaching_asn_points_fp
\fp_new:N \l__tumteaching_asn_bonus_points_fp
\bool_new:N \l__tumteaching_is_homework_asn_bool
\bool_new:N \l__tumteaching_is_lab_asn_bool
\tl_clear_new:N \l__tumteaching_asn_level_tl
\keys_define:nn { tumteaching/asn } {
  points .fp_set:N = \l__tumteaching_asn_points_fp ,
  points .value_required:n = true ,
  bonuspoints .fp_set:N = \l__tumteaching_asn_bonus_points_fp ,
  bonuspoints .value_required:n = true ,
  H .bool_set:N = \l__tumteaching_is_homework_asn_bool ,% H = Homework Assignment
  L .bool_set:N = \l__tumteaching_is_lab_asn_bool ,     % L = Lab Assignment
  level .value_required:n = true,
  level .tl_set:N = \l__tumteaching_asn_level_tl,
}

\bool_new:N \l__tumteaching_show_solution_bool
\bool_new:N \l__tumteaching_has_finish_line_bool

% These strings will be localized according to the class option 'german'
\tl_new:N \g_tumteaching_first_name_tl \tl_gset:Nn \g_tumteaching_first_name_tl {First~Name}
\tl_new:N \g_tumteaching_surname_tl \tl_gset:Nn \g_tumteaching_surname_tl {Surname}
\tl_new:N \g_tumteaching_student_id_tl \tl_gset:Nn \g_tumteaching_student_id_tl {Student~ID}
\tl_new:N \g_tumteaching_signature_tl \tl_gset:Nn \g_tumteaching_signature_tl {Signature}
\tl_new:N \g_tumteaching_deadline_tl \tl_gset:Nn \g_tumteaching_deadline_tl {Deadline}
\tl_new:N \g_tumteaching_asn_title_tl \tl_gset:Nn \g_tumteaching_asn_title_tl {Assignment}         % asn = assignment
\tl_new:N \g_tumteaching_sol_title_tl \tl_gset:Nn \g_tumteaching_sol_title_tl {Suggested~Solution} % sol = solution
\tl_new:N \g_tumteaching_type_exam_tl \tl_gset:Nn \g_tumteaching_type_exam_tl {Exam}
\tl_new:N \g_tumteaching_type_exsheet_tl \tl_gset:Nn \g_tumteaching_type_exsheet_tl {Exercise~Sheet}
\tl_new:N \g_tumteaching_points_tl \tl_gset:Nn \g_tumteaching_points_tl {Points}
\tl_new:N \g_tumteaching_bonus_points_tl \tl_gset:Nn \g_tumteaching_bonus_points_tl {Bonus}
\tl_new:N \g_tumteaching_homework_asn_tl \tl_gset:Nn \g_tumteaching_homework_asn_tl {(H)}
\tl_new:N \g_tumteaching_lab_asn_tl \tl_gset:Nn \g_tumteaching_lab_asn_tl {(L)}
\tl_new:N \g_tumteaching_level_tl \tl_gset:Nn \g_tumteaching_level_tl {Level}
\tl_new:N \g_tumteaching_obj_title_tl \tl_gset:Nn \g_tumteaching_obj_title_tl {Learning~Objectives}
\tl_new:N \g_tumteaching_obj_disclaimer_tl \tl_gset:Nn \g_tumteaching_obj_disclaimer_tl {In~this~week's~exercises,~you~are~going~to~learn:}
\tl_new:N \g_tumteaching_disc_title_tl \tl_gset:Nn \g_tumteaching_disc_title_tl {General~Information}

\tl_new:N \g_tumteaching_lecture_tl
\tl_new:N \g_tumteaching_teacher_tl     % Dozent
\tl_new:N \g_tumteaching_instructor_tl  % Uebungsleitung
\tl_new:N \g_tumteaching_exsheet_nr_tl \tl_gset:Nn \g_tumteaching_exsheet_nr_tl {-1}
\tl_new:N \g_tumteaching_exsheet_deadline_tl
\tl_new:N \g_tumteaching_date_tl
\tl_new:N \g_tumteaching_exam_time_tl \tl_gset:Nn \g_tumteaching_exam_time_tl {-1}
\NewDocumentCommand \pointsToPass { } { \fp_to_decimal:n { ceil(\g__tumteaching_asn_total_points_fp * 0.4) } }
\def\TUMLogo{2015_Logo_TUM_RGB}
\NewDocumentCommand \gradingmatrixBox { } { \makebox(30,45){} }

\NewDocumentCommand \type { }
{
  \bool_if:NTF \g__tumteaching_is_exam_bool
    { \tl_use:N \g_tumteaching_type_exam_tl }
    { \tl_use:N \g_tumteaching_type_exsheet_tl\ \tl_use:N \g_tumteaching_exsheet_nr_tl}
}

\NewDocumentCommand \remarks { }
{
  \begin{itemize}
    \item Fill~out~the~fields~above.
    \item Make~use~only~of~an~indelible~pen~in~black~or~blue~color.
    \item Do~not~make~use~of~``Tipp-Ex''~or~something~similar.
    \item To~solve~the~exam~you~have~\textbf{\tl_use:N \g_tumteaching_exam_time_tl}~minutes.
    \item Check~that~you~received~\textbf{\pageref{LastPage}}~pages.
    \item The~maximum~number~of~points~that~you~can~obtain~in~this~exam~is~\textbf{
          \fp_to_tl:n { \g__tumteaching_asn_total_points_fp + \g__tumteaching_asn_total_bonus_points_fp } }. \\
          You~need~\textbf{\pointsToPass}~points~to~pass.
    \item One~A4~sheet~which~is~handwritten~on~both~sides,~is~allowed.
  \end{itemize}
}

\NewDocumentCommand \remarksGerman { }
{
  \begin{itemize}
    \item F\"ullen~Sie~die~oben~angegebenen~Felder~aus.
    \item Schreiben~Sie~nur~mit~einem~dokumentenechten~Stift~in~schwarzer~oder~blauer~Farbe.
    \item Verwenden~Sie~kein~``Tipp-Ex''~oder~\"ahnliches.
    \item Die~Arbeitszeit~betr\"agt~\textbf{\tl_use:N \g_tumteaching_exam_time_tl}~Minuten.
    \item Pr\"ufen~Sie,~ob~Sie~\textbf{\pageref{LastPage}}~Seiten~erhalten~haben.
    \item Sie~k\"onnen~maximal~\textbf{
          \fp_to_tl:n { \g__tumteaching_asn_total_points_fp  + \g__tumteaching_asn_total_bonus_points_fp }
          }~Punkte~erreichen.~Erreichen~Sie~mindestens~\textbf{\pointsToPass}~Punkte,~bestehen~Sie~die~Klausur.
    \item Als~Hilfsmittel~ist~nur~ein~beidseitig~beschriebenes~A4-Blatt~zugelassen.
    % \item Alle~Funktionen~aus~der~OCaml-Referenz~d\"urfen~ohne~Angabe~von~Modulnamen~verwendet~werden.
    \item Es~d\"urfen~nur~Funktionen~aus~den~Modulen~\textbf{Pervasives},~\textbf{List},~\textbf{String},~\textbf{Thread}~und~\textbf{Event}~verwendet~werden.~Funktionen~aus~diesen~Modulen~d\"urfen~ohne~Angabe~von~Modulnamen~verwendet~werden.
  \end{itemize}
}

\bool_if:NT \g__tumteaching_german_bool
{
  \tl_gset:Nn \g_tumteaching_first_name_tl   {Vorname}
  \tl_gset:Nn \g_tumteaching_surname_tl      {Nachname}
  \tl_gset:Nn \g_tumteaching_student_id_tl   {Matrikelnummer}
  \tl_gset:Nn \g_tumteaching_signature_tl    {Unterschrift}
  \tl_gset:Nn \g_tumteaching_deadline_tl     {Abgabefrist}
  \tl_gset:Nn \g_tumteaching_asn_title_tl    {Aufgabe}
  \tl_gset:Nn \g_tumteaching_sol_title_tl    {L\"osungsvorschlag}
  \tl_gset:Nn \g_tumteaching_type_exam_tl    {Klausur}
  \tl_gset:Nn \g_tumteaching_type_exsheet_tl {\"Ubungsblatt}
  \tl_gset:Nn \g_tumteaching_points_tl       {Punkte}
  \tl_gset:Nn \g_tumteaching_bonus_points_tl {Bonuspunkte}
  \tl_gset:Nn \g_tumteaching_homework_asn_tl {(H)}
  \tl_gset:Nn \g_tumteaching_lab_asn_tl      {(P)}
  \tl_gset:Nn \g_tumteaching_level_tl		 {Stufe}
  \tl_gset:Nn \g_tumteaching_obj_title_tl	 {Lernziele}
  \tl_gset:Nn \g_tumteaching_obj_disclaimer_tl {In~den~\"Ubungen~dieser~Woche~lernen~Sie:}
  \tl_gset:Nn \g_tumteaching_disc_title_tl {Allgemeine~Informationen}

  \RenewDocumentCommand \remarks { } { \remarksGerman }
}

\NewDocumentEnvironment{disclaimer}{m}
{
	\noindent\textbf{#1}\\
}
{
}

\NewDocumentEnvironment{assignment}{O{} m}
{
  \keys_set:nn { tumteaching/asn } { #1 }
  \int_gincr:N \g__tumteaching_asn_number_int
  \fp_gadd:Nn \g__tumteaching_asn_points_fp { \l__tumteaching_asn_points_fp }
  \fp_gadd:Nn \g__tumteaching_asn_bonus_points_fp { \l__tumteaching_asn_bonus_points_fp }
  \bool_if:nTF {\g__tumteaching_show_solution_bool || (\g__tumteaching_show_lab_solution_bool && \l__tumteaching_is_lab_asn_bool)}
    {\bool_set_true:N \l__tumteaching_show_solution_bool}
    {\bool_set_false:N \l__tumteaching_show_solution_bool}

  \pagebreak[2]
  \noindent\textbf{
    \tl_use:N \g_tumteaching_asn_title_tl \c_space_token
    \bool_if:NF \g__tumteaching_is_exam_bool {\tl_use:N \g_tumteaching_exsheet_nr_tl .}
    \int_use:N \g__tumteaching_asn_number_int
    \c_space_token
    \bool_if:NT \l__tumteaching_is_homework_asn_bool {\tl_use:N \g_tumteaching_homework_asn_tl \c_space_token}
    \bool_if:NT \l__tumteaching_is_lab_asn_bool      {\tl_use:N \g_tumteaching_lab_asn_tl \c_space_token}
    #2
  }
  \hfill
  % if points=0 and bonuspoints=0
  \bool_if:nTF { \fp_compare_p:n { \l__tumteaching_asn_points_fp = 0 } && \fp_compare_p:n { \l__tumteaching_asn_bonus_points_fp = 0 } }
	  { \tl_if_empty:NTF \l__tumteaching_asn_level_tl { }
	  	{ \mbox{[\tl_use:N \l__tumteaching_asn_level_tl ]} }
	  %{ \mbox{[\tl_use:N \g_tumteaching_level_tl \c_space_token \tl_use:N \l__tumteaching_asn_level_tl ]} }
	} {}
  % if points!=0 and bonuspoints=0
  \bool_if:nTF { \fp_compare_p:n { \l__tumteaching_asn_points_fp != 0 } && \fp_compare_p:n { \l__tumteaching_asn_bonus_points_fp = 0 } }
    { 
    % if level=0
	  \tl_if_empty:NTF \l__tumteaching_asn_level_tl
	    { \mbox{[\fp_use:N \l__tumteaching_asn_points_fp \c_space_token \tl_use:N \g_tumteaching_points_tl]} } 
	% else
		{ \mbox{[\fp_use:N \l__tumteaching_asn_points_fp \c_space_token \tl_use:N \g_tumteaching_points_tl , \c_space_token \tl_use:N \l__tumteaching_asn_level_tl ]} } 
		%{ \mbox{[\fp_use:N \l__tumteaching_asn_points_fp \c_space_token \tl_use:N \g_tumteaching_points_tl , \c_space_token \tl_use:N \g_tumteaching_level_tl \c_space_token \tl_use:N \l__tumteaching_asn_level_tl ]} } 
	}{ }
  % if points!=0 and bonuspoints!=0
  \bool_if:nTF { \fp_compare_p:n { \l__tumteaching_asn_points_fp != 0 } && \fp_compare_p:n { \l__tumteaching_asn_bonus_points_fp != 0 } }
    { 
    % if level=0
      \tl_if_empty:NTF \l__tumteaching_asn_level_tl
      { \mbox{[\fp_use:N \l__tumteaching_asn_points_fp \c_space_token \tl_use:N \g_tumteaching_points_tl \c_space_token + \c_space_token \fp_use:N \l__tumteaching_asn_bonus_points_fp \c_space_token \tl_use:N \g_tumteaching_bonus_points_tl]} }
  	% else
	  	{ \mbox{[\fp_use:N \l__tumteaching_asn_points_fp \c_space_token \tl_use:N \g_tumteaching_points_tl \c_space_token + \c_space_token \fp_use:N \l__tumteaching_asn_bonus_points_fp \c_space_token \tl_use:N \g_tumteaching_bonus_points_tl , \c_space_token \tl_use:N \l__tumteaching_asn_level_tl]} }
		%{ \mbox{[\fp_use:N \l__tumteaching_asn_points_fp \c_space_token \tl_use:N \g_tumteaching_points_tl \c_space_token + \c_space_token \fp_use:N \l__tumteaching_asn_bonus_points_fp \c_space_token \tl_use:N \g_tumteaching_bonus_points_tl , \c_space_token \tl_use:N \g_tumteaching_level_tl \c_space_token \tl_use:N \l__tumteaching_asn_level_tl]} }
	} {}
  % if points=0 and bonuspoints!=0
  \bool_if:nTF { \fp_compare_p:n { \l__tumteaching_asn_points_fp = 0 } && \fp_compare_p:n { \l__tumteaching_asn_bonus_points_fp != 0 } }
    { 
	% if level=0
	\tl_if_empty:NTF \l__tumteaching_asn_level_tl
	{ \mbox{[\fp_use:N \l__tumteaching_asn_bonus_points_fp \c_space_token \tl_use:N \g_tumteaching_bonus_points_tl]} }
	% else
	{ \mbox{[\fp_use:N \l__tumteaching_asn_bonus_points_fp \c_space_token \tl_use:N \g_tumteaching_bonus_points_tl , \c_space_token \tl_use:N \l__tumteaching_asn_level_tl ]} }
	%{ \mbox{[\fp_use:N \l__tumteaching_asn_bonus_points_fp \c_space_token \tl_use:N \g_tumteaching_bonus_points_tl , \c_space_token \tl_use:N \g_tumteaching_level_tl \c_space_token \tl_use:N \l__tumteaching_asn_level_tl ]} }
	} { }
  \\[3pt]
}{
  \bool_if:NTF \g__tumteaching_is_exam_bool
    { \clearpage }
    { \bigskip }
}

\NewDocumentEnvironment{solution}{}
{
  \bool_if:NTF \l__tumteaching_show_solution_bool
    { \bool_if:NTF \g__tumteaching_is_exam_bool { \clearpage } { \bigskip \pagebreak[2]}
      \noindent\textbf{
      \tl_use:N \g_tumteaching_sol_title_tl \c_space_token
      \bool_if:NF \g__tumteaching_is_exam_bool {\tl_use:N \g_tumteaching_exsheet_nr_tl .}
      \int_use:N \g__tumteaching_asn_number_int}\\[3pt]
    }{ \setbox0=\vbox\bgroup\let\ref\relax\let\eqref\relax }
}{
  \bool_if:NTF \l__tumteaching_show_solution_bool
    { \bool_if:NTF \g__tumteaching_is_exam_bool { \clearpage } { \bigskip } }
    { \egroup }
}

\NewDocumentCommand \IfSolution { m }
{
  \bool_if:NT \l__tumteaching_show_solution_bool { #1 }
}

\NewDocumentCommand \IfNoSolution { m }
{
  \bool_if:NF \l__tumteaching_show_solution_bool { #1 }
}

\NewDocumentCommand \byes { }
{
  \bool_if:NTF \l__tumteaching_show_solution_bool
    {\mbox{{\LARGE$\boxtimes$}\quad{\LARGE$\square$}}}
    {\mbox{{\LARGE$\square$}\quad{\LARGE$\square$}}}
}
\NewDocumentCommand \bno { }
{
  \bool_if:NTF \l__tumteaching_show_solution_bool
    {\mbox{{\LARGE$\square$}\quad{\LARGE$\boxtimes$}}}
    {\mbox{{\LARGE$\square$}\quad{\LARGE$\square$}}}
}
\NewDocumentCommand \bboxtrue { }
{
  \bool_if:NTF \l__tumteaching_show_solution_bool
    {\raisebox{-.7ex}{\LARGE$\boxtimes$}}
    {\raisebox{-.7ex}{\LARGE$\square$}}
}
\NewDocumentCommand \bboxfalse { }
{
  \bool_if:NTF \l__tumteaching_show_solution_bool
    {\raisebox{-.7ex}{\LARGE$\square$}}
    {\raisebox{-.7ex}{\LARGE$\square$}}
}

\NewDocumentCommand \answerline { m m }
{
  \\ \\
  \bool_if:NTF \l__tumteaching_show_solution_bool
    { \hbox to 0pt {\hbox to #1 {\hrulefill}} \raise .7ex \hbox to #1 { #2 \hss } }
    { \hbox to #1 {\hrulefill} }
}

\NewDocumentCommand \answerbox { m m }
{
  \bool_if:NTF \l__tumteaching_show_solution_bool
    {\fbox { \raisebox{0.1ex}{ #2 } } }
    {\fbox { \raisebox{2.1ex}{ ~\hspace{#1}~ } } }
}

\NewDocumentCommand \dottedline { m }
{
  % \rule{.8ex}{.5pt}
  \raisebox{-.3ex}{\makebox[#1]{\xleaders\hbox{\,\rule{1pt}{1pt}}\hfill}}
}
% \def\dashfill{\cleaders\hbox to 2em{-}\hfill}

\NewDocumentCommand \gradingmatrix { }
{
  \begin{tabular}{@{}|*{\int_eval:n { \g__tumteaching_asn_total_numbers_int + 1 }}{c|}@{}}
  \hline
  \int_zero:N \g_tmpa_int
  \prg_replicate:nn { \int_eval:n {\g__tumteaching_asn_total_numbers_int} } { \int_gincr:N \g_tmpa_int \int_to_arabic:n { \g_tmpa_int } & } $\Sigma$ \\\hline
  \int_zero:N \g_tmpa_int
  \prg_replicate:nn { \int_eval:n {\g__tumteaching_asn_total_numbers_int} } { \int_gincr:N \g_tmpa_int \gradingmatrixBox & } \gradingmatrixBox \\\hline
  \int_zero:N \g_tmpa_int
  \prg_replicate:nn { \int_eval:n {\g__tumteaching_asn_total_numbers_int} } { \int_gincr:N \g_tmpa_int \gradingmatrixBox & } \gradingmatrixBox \\\hline
  \end{tabular}
}

\NewDocumentCommand \maketitleExam { }
{
  \noindent\textbf{\Large\tl_use:N \g_tumteaching_lecture_tl} \hfill \includegraphics[height=10mm]{\TUMLogo} \\[1pt]
  \noindent\rule[0.5ex]{\linewidth}{1pt} \\
  \tl_use:N \g_tumteaching_teacher_tl \tl_use:N \g_tumteaching_instructor_tl \hfill \tl_use:N \g_tumteaching_date_tl \\[3pt]
  \textbf{\type}
  \par
  \bigskip

  \begin{center}
    \begin{tabular}{@{}|p{.47\linewidth}|p{.47\linewidth}|@{}}
      \hline
      \tl_use:N\g_tumteaching_first_name_tl & \tl_use:N\g_tumteaching_surname_tl \\[40pt]\hline
      \tl_use:N\g_tumteaching_student_id_tl & \tl_use:N\g_tumteaching_signature_tl \\[40pt]\hline
    \end{tabular}
  \end{center}

  \vfill
  \remarks
  \vfill
  \bool_if:NTF \g__tumteaching_german_bool
    { vorzeitige~Abgabe~um~\dottedline{2cm} \hfill H\"orsaal~verlassen~von~\dottedline{2cm}~bis~\dottedline{2cm} \\[3mm] }
    { premature~hand~in~\dottedline{2cm} \hfill rest~room~break~from~\dottedline{2cm}~until~\dottedline{2cm} \\[3mm] }
   \rule[0.5ex]{\linewidth}{2pt}
  \\[5mm]
  \makebox[\textwidth][c]{
    \begin{minipage}{\paperwidth}
      \centering\gradingmatrix%
       \bool_if:NTF \g__tumteaching_german_bool
         { \begin{tabular}{@{\quad}c} \dottedline{3cm} \\ {\small Erstkorrektor} \\[1cm] \dottedline{3cm} \\ {\small Zweitkorrektor} \end{tabular} }
         { \begin{tabular}{@{\quad}c} \dottedline{3cm} \\ {\small First~Reviser} \\[1cm] \dottedline{3cm} \\ {\small Second~Reviser} \end{tabular} }
    \end{minipage}
  }
  \clearpage
}

\NewDocumentCommand \maketitleExSheet { }
{
  \noindent\textbf{\Large\tl_use:N \g_tumteaching_lecture_tl} \hfill \includegraphics[height=10mm]{\TUMLogo} \\[1pt]
  \noindent\rule[0.5ex]{\linewidth}{1pt} \\
  \tl_use:N \g_tumteaching_teacher_tl \tl_use:N \g_tumteaching_instructor_tl \hfill \tl_use:N \g_tumteaching_date_tl \\[3pt]
  \textbf{\type} \hfill
  \tl_if_empty:NTF \g_tumteaching_exsheet_deadline_tl
    { }
    { \tl_use:N \g_tumteaching_deadline_tl : \c_space_token \tl_use:N \g_tumteaching_exsheet_deadline_tl }
  \bool_if:NT \g__tumteaching_show_point_sum_bool
    { \\[3pt]\null\hfill$\Sigma$~Punkte:~\fp_to_decimal:N\g__tumteaching_asn_total_points_fp \quad$|$\quad $\Sigma$~Bonuspunkte:~\fp_to_decimal:N\g__tumteaching_asn_total_bonus_points_fp }
  \par
  \bigskip
}

\RenewDocumentCommand \maketitle { }
{
  \bool_if:NTF \g__tumteaching_is_exam_bool
    { \maketitleExam }
    { \maketitleExSheet }
}

\AfterEndPreamble { \maketitle }

\AtEndDocument {
  \iow_now:cx { @auxout }
  {
    \token_to_str:N \ExplSyntaxOn
    ^^J
    \int_gset:Nn \g__tumteaching_asn_total_numbers_int { \int_use:N \g__tumteaching_asn_number_int }
    ^^J
    \fp_gset:Nn \exp_not:N \g__tumteaching_asn_total_points_fp { \fp_to_decimal:N \g__tumteaching_asn_points_fp }
    ^^J
    \fp_gset:Nn \exp_not:N \g__tumteaching_asn_total_bonus_points_fp { \fp_to_decimal:N \g__tumteaching_asn_bonus_points_fp }
    ^^J
    \token_to_str:N \ExplSyntaxOff
  }
}
