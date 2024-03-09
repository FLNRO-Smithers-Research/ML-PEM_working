<map version="freeplane 1.6.0">
<!--To view this file, download free mind mapping software Freeplane from http://freeplane.sourceforge.net -->
<node TEXT="PEM File Structure" FOLDED="false" ID="ID_215222689" CREATED="1586974813119" MODIFIED="1586974828372" STYLE="oval">
<font SIZE="18"/>
<hook NAME="MapStyle">
    <properties edgeColorConfiguration="#808080ff,#ff0000ff,#0000ffff,#00ff00ff,#ff00ffff,#00ffffff,#7c0000ff,#00007cff,#007c00ff,#7c007cff,#007c7cff,#7c7c00ff" fit_to_viewport="false"/>

<map_styles>
<stylenode LOCALIZED_TEXT="styles.root_node" STYLE="oval" UNIFORM_SHAPE="true" VGAP_QUANTITY="24.0 pt">
<font SIZE="24"/>
<stylenode LOCALIZED_TEXT="styles.predefined" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="default" ICON_SIZE="12.0 pt" COLOR="#000000" STYLE="fork">
<font NAME="SansSerif" SIZE="10" BOLD="false" ITALIC="false"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.details"/>
<stylenode LOCALIZED_TEXT="defaultstyle.attributes">
<font SIZE="9"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.note" COLOR="#000000" BACKGROUND_COLOR="#ffffff" TEXT_ALIGN="LEFT"/>
<stylenode LOCALIZED_TEXT="defaultstyle.floating">
<edge STYLE="hide_edge"/>
<cloud COLOR="#f0f0f0" SHAPE="ROUND_RECT"/>
</stylenode>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.user-defined" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="styles.topic" COLOR="#18898b" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subtopic" COLOR="#cc3300" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subsubtopic" COLOR="#669900">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.important">
<icon BUILTIN="yes"/>
</stylenode>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.AutomaticLayout" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="AutomaticLayout.level.root" COLOR="#000000" STYLE="oval" SHAPE_HORIZONTAL_MARGIN="10.0 pt" SHAPE_VERTICAL_MARGIN="10.0 pt">
<font SIZE="18"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,1" COLOR="#0033ff">
<font SIZE="16"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,2" COLOR="#00b439">
<font SIZE="14"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,3" COLOR="#990000">
<font SIZE="12"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,4" COLOR="#111111">
<font SIZE="10"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,5"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,6"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,7"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,8"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,9"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,10"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,11"/>
</stylenode>
</stylenode>
</map_styles>
</hook>
<hook NAME="AutomaticEdgeColor" COUNTER="8" RULE="ON_BRANCH_CREATION"/>
<node TEXT="DefinitionTable.csv" POSITION="right" ID="ID_126434660" CREATED="1586976782118" MODIFIED="1586977131761">
<edge COLOR="#007c00"/>
<richcontent TYPE="DETAILS">

<html>
  <head>
    
  </head>
  <body>
    <p>
      <i>or .xlsx </i>
    </p>
    <p>
      <i>- predict </i>
    </p>
    <p>
      <i>- input set</i>
    </p>
    <p>
      <i>- covariate set</i>
    </p>
  </body>
</html>

</richcontent>
</node>
<node TEXT="/[Predict Class]/" POSITION="right" ID="ID_1881093100" CREATED="1586975132303" MODIFIED="1586976741208">
<edge COLOR="#0000ff"/>
<node TEXT="[cov set + input set]/" ID="ID_1998931590" CREATED="1586975136655" MODIFIED="1586977689008" HGAP_QUANTITY="8.750000156462189 pt" VSHIFT_QUANTITY="48.749998547136826 pt"/>
<node TEXT="cv1-ip1/" ID="ID_114448433" CREATED="1586975632958" MODIFIED="1586976975672">
<node TEXT="model_report.html" ID="ID_1917540648" CREATED="1586976892910" MODIFIED="1586976975671" HGAP_QUANTITY="12.500000044703482 pt" VSHIFT_QUANTITY="23.999999284744284 pt"/>
<node TEXT="validation.RDS" ID="ID_307945805" CREATED="1586976917934" MODIFIED="1586976942624"/>
<node TEXT="model.RDS" ID="ID_1298322610" CREATED="1586976943342" MODIFIED="1586976950999"/>
<node TEXT="rasters" ID="ID_705823363" CREATED="1586976951574" MODIFIED="1586977058535">
<node TEXT="prob.VarSS01" ID="ID_1458525078" CREATED="1586976981910" MODIFIED="1586977058535" VSHIFT_QUANTITY="31.499999061226873 pt"/>
<node TEXT="prob.VarSS02" ID="ID_397815928" CREATED="1586977020870" MODIFIED="1586977030016"/>
<node TEXT="..." ID="ID_1212152468" CREATED="1586977030631" MODIFIED="1586977033136"/>
<node TEXT="response" ID="ID_702492187" CREATED="1586977033822" MODIFIED="1586977052297"><richcontent TYPE="DETAILS">

<html>
  <head>
    
  </head>
  <body>
    <p>
      <i>multiclass</i>
    </p>
  </body>
</html>

</richcontent>
</node>
</node>
</node>
<node TEXT="cv1-ip2/" ID="ID_474735357" CREATED="1586975645263" MODIFIED="1586976754072"/>
<node TEXT="cv2-ip1/" ID="ID_526570758" CREATED="1586975654278" MODIFIED="1586976756935"/>
<node TEXT="..." ID="ID_84038770" CREATED="1586975660750" MODIFIED="1586975662228"/>
<node TEXT="cv10-ip5/" ID="ID_681423084" CREATED="1586975662822" MODIFIED="1586976760144"/>
</node>
<node TEXT="/ForestNonF/" POSITION="right" ID="ID_1135986131" CREATED="1586975690854" MODIFIED="1586976767088">
<edge COLOR="#ff00ff"/>
</node>
<node TEXT="/SiteSeries/" POSITION="right" ID="ID_1310812412" CREATED="1586975700558" MODIFIED="1586976771128">
<edge COLOR="#00ffff"/>
</node>
<node TEXT="/GrpdSiteSeries/" POSITION="right" ID="ID_1175179827" CREATED="1586975708055" MODIFIED="1586976775775">
<edge COLOR="#7c0000"/>
</node>
<node TEXT="..." POSITION="right" ID="ID_687251044" CREATED="1586976776503" MODIFIED="1586976778696">
<edge COLOR="#00007c"/>
</node>
<node TEXT="Levers" POSITION="left" ID="ID_955990994" CREATED="1586975183782" MODIFIED="1586975193959">
<edge COLOR="#00ff00"/>
<node TEXT="Input Training Set" ID="ID_1638551120" CREATED="1586975290542" MODIFIED="1586975297935"/>
<node TEXT="Covariate Set" ID="ID_575481249" CREATED="1586975299303" MODIFIED="1586975307016"/>
<node TEXT="Model Set" ID="ID_55847082" CREATED="1586975309583" MODIFIED="1586975590549">
<node TEXT="Learner definition" ID="ID_1019556193" CREATED="1586975560967" MODIFIED="1586975590548" HGAP_QUANTITY="13.250000022351742 pt" VSHIFT_QUANTITY="17.99999946355821 pt"/>
<node TEXT="validation definition" ID="ID_285767165" CREATED="1586975572198" MODIFIED="1586975578464"/>
<node TEXT="model generation" ID="ID_1133502200" CREATED="1586975579886" MODIFIED="1586975585328"/>
</node>
</node>
</node>
</map>
