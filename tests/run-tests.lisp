;;;;  HTML5 parser for Common Lisp
;;;;
;;;;  Copyright (C) 2012 Thomas Bakketun <thomas.bakketun@copyleft.no>
;;;;  Copyright (C) 2012 Asgeir Bjørlykke <asgeir@copyleft.no>
;;;;  Copyright (C) 2012 Mathias Hellevang
;;;;  Copyright (C) 2012 Stian Sletner <stian@copyleft.no>
;;;;
;;;;  This library is free software: you can redistribute it and/or modify
;;;;  it under the terms of the GNU Lesser General Public License as published
;;;;  by the Free Software Foundation, either version 3 of the License, or
;;;;  (at your option) any later version.
;;;;
;;;;  This library is distributed in the hope that it will be useful,
;;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;  GNU General Public License for more details.
;;;;
;;;;  You should have received a copy of the GNU General Public License
;;;;  along with this library.  If not, see <http://www.gnu.org/licenses/>.

(in-package :html5-parser-tests)

(defparameter *skip-all-errors* nil)
;(defparameter *skip-all-errors* t)
(defvar *skipped-errors*)

(defparameter *known-failures*
  '(
    "Skip test tests3 <!doctype html><html><body><p><table></table></body></html>"
    "Skip test adoption01 <b><em><foo><foob><fooc><aside></b></em>"
    "Skip test blocks <!doctype html><p>foo<dialog>bar<p>baz"
    "Skip test blocks <!doctype html><dialog><p>foo</dialog>bar"
    "Skip test foreign-fragment <nobr>X"
    "Skip test foreign-fragment <font color></font>X"
    "Skip test foreign-fragment <font></font>X"
    "Skip test foreign-fragment <g></path>X"
    "Skip test foreign-fragment <b></b><mglyph/><i></i><malignmark/><u></u><ms/>X"
    "Skip test foreign-fragment <malignmark></malignmark>"
    "Skip test foreign-fragment <b></b><mglyph/><i></i><malignmark/><u></u><mn/>X"
    "Skip test foreign-fragment <malignmark></malignmark>"
    "Skip test foreign-fragment <b></b><mglyph/><i></i><malignmark/><u></u><mo/>X"
    "Skip test foreign-fragment <malignmark></malignmark>"
    "Skip test foreign-fragment <b></b><mglyph/><i></i><malignmark/><u></u><mi/>X"
    "Skip test foreign-fragment <malignmark></malignmark>"
    "Skip test foreign-fragment <b></b><mglyph/><i></i><malignmark/><u></u><mtext/>X"
    "Skip test foreign-fragment <malignmark></malignmark>"
    "Skip test foreign-fragment <div></div>"
    "Skip test foreign-fragment <figure></figure>"
    "Skip test foreign-fragment <div></div>"
    "Skip test foreign-fragment <figure></figure>"
    "Skip test foreign-fragment <div><h1>X</h1></div>"
    "Skip test foreign-fragment <div></div>" "Skip test isindex <isindex>"
    "Skip test isindex <isindex name=\"A\" action=\"B\" prompt=\"C\" foo=\"D\">"
    "Skip test isindex <form><isindex>"
    "Skip test isindex <!doctype html><isindex>x</isindex>x"
    "Skip test main-element <!doctype html><p>foo<main>bar<p>baz"
    "Skip test main-element <!doctype html><main><p>foo</main>bar"
    "Skip test math <math><tr><td><mo><tr>"
    "Skip test math <math><tr><td><mo><tr>"
    "Skip test math <math><thead><mo><tbody>"
    "Skip test math <math><tfoot><mo><tbody>"
    "Skip test math <math><tbody><mo><tfoot>"
    "Skip test namespace-sensitivity <body><table><tr><td><svg><td><foreignObject><span></td>Foo"
    "Skip test noscript01 <head><noscript><!doctype html><!--foo--></noscript>"
    "Skip test noscript01 <head><noscript><html class=\"foo\"><!--foo--></noscript>"
    "Skip test noscript01 <head><noscript><!--foo--></noscript>"
    "Skip test noscript01 <head><noscript><basefont><!--foo--></noscript>"
    "Skip test noscript01 <head><noscript><bgsound><!--foo--></noscript>"
    "Skip test noscript01 <head><noscript><link><!--foo--></noscript>"
    "Skip test noscript01 <head><noscript><meta><!--foo--></noscript>"
    "Skip test noscript01 <head><noscript><noframes>XXX</noscript></noframes></noscript>"
    "Skip test noscript01 <head><noscript><style>XXX</style></noscript>"
    "Skip test noscript01 <head><noscript></br><!--foo--></noscript>"
    "Skip test noscript01 <head><noscript><head class=\"foo\"><!--foo--></noscript>"
    "Skip test noscript01 <head><noscript><noscript class=\"foo\"><!--foo--></noscript>"
    "Skip test noscript01 <head><noscript></p><!--foo--></noscript>"
    "Skip test noscript01 <head><noscript><p><!--foo--></noscript>"
    "Skip test noscript01 <head><noscript>XXX<!--foo--></noscript></head>"
    "Skip test ruby <html><ruby>a<rb>b<rb></ruby></html>"
    "Skip test ruby <html><ruby>a<rb>b<rt></ruby></html>"
    "Skip test ruby <html><ruby>a<rb>b<rtc></ruby></html>"
    "Skip test ruby <html><ruby>a<rb>b<rp></ruby></html>"
    "Skip test ruby <html><ruby>a<rt>b<rb></ruby></html>"
    "Skip test ruby <html><ruby>a<rt>b<rtc></ruby></html>"
    "Skip test ruby <html><ruby>a<rtc>b<rb></ruby></html>"
    "Skip test ruby <html><ruby>a<rtc>b<rtc></ruby></html>"
    "Skip test ruby <html><ruby>a<rp>b<rb></ruby></html>"
    "Skip test ruby <html><ruby>a<rp>b<rtc></ruby></html>"
    "Skip test ruby <html><ruby><rtc><ruby>a<rb>b<rt></ruby></ruby></html>"
    "Skip test template <body><template>Hello</template>"
    "Skip test template <template>Hello</template>"
    "Skip test template <template></template><div></div>"
    "Skip test template <html><template>Hello</template>"
    "Skip test template <head><template><div></div></template></head>"
    "Skip test template <div><template><div><span></template><b>"
    "Skip test template <div><template></div>Hello"
    "Skip test template <table><template></template></table>"
    "Skip test template <table><template></template></div>"
    "Skip test template <table><div><template></template></div>"
    "Skip test template <table><template></template><div></div>"
    "Skip test template <table>   <template></template></table>"
    "Skip test template <table><tbody><template></template></tbody>"
    "Skip test template <table><tbody><template></tbody></template>"
    "Skip test template <table><tbody><template></template></tbody></table>"
    "Skip test template <table><thead><template></template></thead>"
    "Skip test template <table><tfoot><template></template></tfoot>"
    "Skip test template <select><template></template></select>"
    "Skip test template <select><template><option></option></template></select>"
    "Skip test template <template><option></option></select><option></option></template>"
    "Skip test template <select><template></template><option></select>"
    "Skip test template <select><option><template></template></select>"
    "Skip test template <select><template>"
    "Skip test template <select><option></option><template>"
    "Skip test template <select><option></option><template><option>"
    "Skip test template <table><thead><template><td></template></table>"
    "Skip test template <table><template><thead></template></table>"
    "Skip test template <body><table><template><td></tr><div></template></table>"
    "Skip test template <table><template><thead></template></thead></table>"
    "Skip test template <table><thead><template><tr></template></table>"
    "Skip test template <table><template><tr></template></table>"
    "Skip test template <table><tr><template><td>"
    "Skip test template <table><template><tr><template><td></template></tr></template></table>"
    "Skip test template <table><template><tr><template><td></td></template></tr></template></table>"
    "Skip test template <table><template><td></template>"
    "Skip test template <body><template><td></td></template>"
    "Skip test template <body><template><template><tr></tr></template><td></td></template>"
    "Skip test template <table><colgroup><template><col>"
    "Skip test template <template><frame></frame></frameset><frame></frame></template>"
    "Skip test template <template><div><frameset><span></span></div><span></span></template>"
    "Skip test template <body><template><div><frameset><span></span></div><span></span></template></body>"
    "Skip test template <body><template><script>var i = 1;</script><td></td></template>"
    "Skip test template <body><template><tr><div></div></tr></template>"
    "Skip test template <body><template><tr></tr><td></td></template>"
    "Skip test template <body><template><td></td></tr><td></td></template>"
    "Skip test template <body><template><td></td><tbody><td></td></template>"
    "Skip test template <body><template><td></td><caption></caption><td></td></template>"
    "Skip test template <body><template><td></td><colgroup></caption><td></td></template>"
    "Skip test template <body><template><td></td></table><td></td></template>"
    "Skip test template <body><template><tr></tr><tbody><tr></tr></template>"
    "Skip test template <body><template><tr></tr><caption><tr></tr></template>"
    "Skip test template <body><template><tr></tr></table><tr></tr></template>"
    "Skip test template <body><template><thead></thead><caption></caption><tbody></tbody></template>"
    "Skip test template <body><template><thead></thead></table><tbody></tbody></template></body>"
    "Skip test template <body><template><div><tr></tr></div></template>"
    "Skip test template <body><template><em>Hello</em></template>"
    "Skip test template <body><template><!--comment--></template>"
    "Skip test template <body><template><style></style><td></td></template>"
    "Skip test template <body><template><meta><td></td></template>"
    "Skip test template <body><template><link><td></td></template>"
    "Skip test template <body><template><template><tr></tr></template><td></td></template>"
    "Skip test template <body><table><colgroup><template><col></col></template></colgroup></table></body>"
    "Skip test template <body a=b><template><div></div><body c=d><div></div></body></template></body>"
    "Skip test template <html a=b><template><div><html b=c><span></template>"
    "Skip test template <html a=b><template><col></col><html b=c><col></col></template>"
    "Skip test template <html a=b><template><frame></frame><html b=c><frame></frame></template>"
    "Skip test template <body><template><tr></tr><template></template><td></td></template>"
    "Skip test template <body><template><thead></thead><template><tr></tr></template><tr></tr><tfoot></tfoot></template>"
    "Skip test template <body><template><template><b><template></template></template>text</template>"
    "Skip test template <body><template><col><colgroup>"
    "Skip test template <body><template><col></colgroup>"
    "Skip test template <body><template><col><colgroup></template></body>"
    "Skip test template <body><template><col><div>"
    "Skip test template <body><template><col></div>"
    "Skip test template <body><template><col>Hello"
    "Skip test template <body><template><i><menu>Foo</i>"
    "Skip test template <body><template></div><div>Foo</div><template></template><tr></tr>"
    "Skip test template <body><div><template></div><tr><td>Foo</td></tr></template>"
    "Skip test template <template></figcaption><sub><table></table>"
    "Skip test template <template><template>" "Skip test template <template><div>"
    "Skip test template <template><template><div>"
    "Skip test template <template><template><table>"
    "Skip test template <template><template><tbody>"
    "Skip test template <template><template><tr>"
    "Skip test template <template><template><td>"
    "Skip test template <template><template><caption>"
    "Skip test template <template><template><colgroup>"
    "Skip test template <template><template><col>"
    "Skip test template <template><template><tbody><select>"
    "Skip test template <template><template><table>Foo"
    "Skip test template <template><template><frame>"
    "Skip test template <template><template><script>var i"
    "Skip test template <template><template><style>var i"
    "Skip test template <template><table></template><body><span>Foo"
    "Skip test template <template><td></template><body><span>Foo"
    "Skip test template <template><object></template><body><span>Foo"
    "Skip test template <template><svg><template>"
    "Skip test template <template><svg><foo><template><foreignObject><div></template><div>"
    "Skip test template <dummy><template><span></dummy>"
    "Skip test template <body><table><tr><td><select><template>Foo</template><caption>A</table>"
    "Skip test template <body></body><template>"
    "Skip test template <head></head><template>"
    "Skip test template <head></head><template>Foo</template>"
    "Skip test template <!DOCTYPE HTML><dummy><table><template><table><template><table><script>"
    "Skip test template <template><a><table><a>"
    "Skip test tests11 <!DOCTYPE html><body><svg attributename='' attributetype='' basefrequency='' baseprofile='' calcmode='' clippathunits='' diffuseconstant='' edgemode='' filterunits='' filterres='' glyphref='' gradienttransform='' gradientunits='' kernelmatrix='' kernelunitlength='' keypoints='' keysplines='' keytimes='' lengthadjust='' limitingconeangle='' markerheight='' markerunits='' markerwidth='' maskcontentunits='' maskunits='' numoctaves='' pathlength='' patterncontentunits='' patterntransform='' patternunits='' pointsatx='' pointsaty='' pointsatz='' preservealpha='' preserveaspectratio='' primitiveunits='' refx='' refy='' repeatcount='' repeatdur='' requiredextensions='' requiredfeatures='' specularconstant='' specularexponent='' spreadmethod='' startoffset='' stddeviation='' stitchtiles='' surfacescale='' systemlanguage='' tablevalues='' targetx='' targety='' textlength='' viewbox='' viewtarget='' xchannelselector='' ychannelselector='' zoomandpan=''></svg>"
    "Skip test tests11 <!DOCTYPE html><body><svg contentScriptType='' contentStyleType='' externalResourcesRequired='' filterRes=''></svg>"
    "Skip test tests11 <!DOCTYPE html><body><svg CONTENTSCRIPTTYPE='' CONTENTSTYLETYPE='' EXTERNALRESOURCESREQUIRED='' FILTERRES=''></svg>"
    "Skip test tests11 <!DOCTYPE html><body><svg contentscripttype='' contentstyletype='' externalresourcesrequired='' filterres=''></svg>"
    "Skip test tests16 <!doctype html><noscript><!--<noscript></noscript>--></noscript>"
    "Skip test tests16 <!doctype html><noscript><!--</noscript>X<noscript>--></noscript>"
    "Skip test tests16 <!doctype html><noscript><iframe></noscript>X"
    "Skip test tests16 <noscript><!--<noscript></noscript>--></noscript>"
    "Skip test tests16 <noscript><!--</noscript>X<noscript>--></noscript>"
    "Skip test tests16 <noscript><iframe></noscript>X"
    "Skip test tests18 <!doctype html><html><noscript><plaintext></plaintext>"
    "Skip test tests18 <!doctype html><template><plaintext>a</template>b"
    "Skip test tests19 <!doctype html><isindex type=\"hidden\">"
    "Skip test tests19 <html><ruby>a<rb>b<rt></ruby></html>"
    "Skip test tests19 <html><ruby>a<rtc>b<rt>c<rb>d</ruby></html>"
    "Skip test tests2 <!DOCTYPE html><frameset> te st"
    "Skip test tests2 <!DOCTYPE html><frameset></frameset> te st"
    "Skip test tests21 <!DOCTYPE html><svg><![CDATA[foo]]]>"
    "Skip test tests21 <!DOCTYPE html><svg><![CDATA[foo]]]]>"
    "Skip test tests25 <!DOCTYPE html><body><command>A"
    "Skip test tests5 <noscript><!--</noscript>--></noscript>"
    "Skip test tests8 <table><li><li></table>"
    "Skip test webkit02 <p id=\"status\"><noscript><strong>A</strong></noscript><span>B</span></p>"
    "Skip test webkit02 <b><em><foo><foo><foo><aside></b></em>"
    "Skip test webkit02 <b><em><foo><foo><foo><foo><foo><foo><foo><foo><foo><foo><aside></b></em>"
    "Skip test webkit02 <b><em><foo><foob><foob><foob><foob><fooc><fooc><fooc><fooc><food><aside></b></em>"
    ;; test-parser incorrect errors
    "Skip test webkit02 </foreignObject><plaintext><div>foo</div>"
    "Skip test webkit02 <svg><foreignObject></foreignObject><title></svg>foo"
    "Skip test webkit02 <svg><foreignObject><div>foo</div><plaintext></foreignObject></svg><div>bar</div>"
    "Skip test webkit02 <option><XH<optgroup></optgroup>"
    "Skip test webkit02 <b><em><foo><foo><foo><aside></b>"
    "Skip test webkit02 <b><em><foo><foo><aside></b></em>"
    "Skip test webkit02 <b><em><foo><foo><aside></b>"
    "Skip test webkit02 <table><input>" "Skip test webkit02 <legend>test</legend>"
    "Skip test webkit01 <table><tr><td><svg><desc><td></desc><circle>"
    "Skip test tests9 <!DOCTYPE html><body><table><caption><math><mi>foo</mi><mi>bar</mi></math><p>baz</caption></table>"
    "Skip test tests6 <!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\"><html></html>"
    "Skip test tests19 <!doctype html><div></body><!--foo-->"
    "Skip test tests15 <!doctype html><table>X<style> <tr>x </style> </table>"
    "Skip test tests15 <!doctype html><table><tr> x</table>"
    "Skip test tests15 <!doctype html><table> x </table>"
    "Skip test tests15 <!doctype html><table> x</table>"
    "Skip test tests12 <!DOCTYPE html><body>foo<math><mtext><i>baz</i></mtext><annotation-xml><svg><desc><b>eggs</b></desc><g><foreignObject><P>spam<TABLE><tr><td><img></td></table></foreignObject></g><g>quux</g></svg></annotation-xml></math>bar"
    "Skip test tests12 <!DOCTYPE html><body><p>foo<math><mtext><i>baz</i></mtext><annotation-xml><svg><desc><b>eggs</b></desc><g><foreignObject><P>spam<TABLE><tr><td><img></td></table></foreignObject></g><g>quux</g></svg></annotation-xml></math>bar"
    "Skip test tests10 <!DOCTYPE html><body><table><caption><svg><g>foo</g><g>bar</g></svg><p>baz</caption></table>"
    "Skip test tests1 <!DOCTYPE html><li>hello<li>world<ul>how<li>do</ul>you</body><!--do-->"
    "Skip test menuitem-element <!DOCTYPE html><menuitem></html>"
    "Skip test menuitem-element <!DOCTYPE html><menuitem></body>"
    "Skip test math <math><tfoot><mo></table>"
    "Skip test math <math><thead><mo></table>"
    "Skip test math <math><tbody><mo></table>"
    "Skip test entities02 <div>ZZ&AElig=</div>"
    "Skip test doctype01 <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
   \"http://www.w3.org/TR/html4/strict.dtd\">Hello"
    "Skip test svg <svg><tr><td><title><tr>"
    "Skip test svg <svg><tr><td><title><tr>"
    "Skip test svg <svg><thead><title><tbody>"
    "Skip test svg <svg><tfoot><title><tbody>"
    "Skip test svg <svg><tbody><title><tfoot>"
    "Skip test svg <svg><tbody><title></table>"
    "Skip test svg <svg><thead><title></table>"
    "Skip test svg <svg><tfoot><title></table>"

    ;; New problems
    "Skip test comments01 FOO<!-- BAR --! >BAZ"
    "Skip test comments01 FOO<!-- BAR --!
>BAZ"
    "Skip test comments01 FOO<!-- BAR --   >BAZ"
    "Skip test comments01 FOO<!-- BAR -- <QUX> -- MUX -- >BAZ"
    "Skip test comments01 <?xml version"
    "Skip test doctype01 <!DOCTYPE root-element [SYSTEM OR PUBLIC FPI] \"uri\" [ 
<!-- internal declarations -->
]>"
    "Skip test domjs-unsafe <svg><![CDATA[foo
bar]]>"
    "Skip test domjs-unsafe <svg><![CDATA[foobar]]>"
    "Skip test domjs-unsafe <svg><![CDATA[foo
bar]]>"
    "Skip test domjs-unsafe <table><colgroup> foo</colgroup></table>"
    "Skip test entities01 FOO&#11111111111"
    "Skip test entities01 FOO&#1111111111"
    "Skip test entities01 FOO&#111111111111"
    "Skip test plain-text-unsafe <svg><![CDATA[ filler text ]]>"
    "Skip test plain-text-unsafe <!DOCTYPE html><pre>

A</pre>"
    "Skip test plain-text-unsafe <!DOCTYPE html><pre>A</pre>"
    "Skip test plain-text-unsafe <!DOCTYPE html><pre>A</pre>"
    "Skip test tests1 <" "Skip test tests1 </" "Skip test tests1 </#"
    "Skip test tests1 <?" "Skip test tests1 <?#" "Skip test tests1 <!"
    "Skip test tests1 <!#"
    "Skip test tests1 <!DOCTYPE html><script> <!-- </script> --> </script> EOF"
    "Skip test tests1 <style><!--</style><meta><script>--><link></script>"
    "Skip test tests1 <head><meta></head><link>"
    "Skip test tests1 <body><body><base><link><meta><title><p></title><body><p></body>"
    "Skip test tests1 <head></p><meta><p>"
    "Skip test tests1 <head></html><meta><p>"
    "Skip test tests10 <!DOCTYPE html><body xlink:href=foo xml:lang=en><svg><g xml:lang=en xlink:href=foo /></svg>"
    "Skip test tests10 <!DOCTYPE html><body xlink:href=foo xml:lang=en><svg><g xml:lang=en xlink:href=foo />bar</svg>"
    "Skip test tests11 <!DOCTYPE html><body><svg><altGlyph /><altGlyphDef /><altGlyphItem /><animateColor /><animateMotion /><animateTransform /><clipPath /><feBlend /><feColorMatrix /><feComponentTransfer /><feComposite /><feConvolveMatrix /><feDiffuseLighting /><feDisplacementMap /><feDistantLight /><feFlood /><feFuncA /><feFuncB /><feFuncG /><feFuncR /><feGaussianBlur /><feImage /><feMerge /><feMergeNode /><feMorphology /><feOffset /><fePointLight /><feSpecularLighting /><feSpotLight /><feTile /><feTurbulence /><foreignObject /><glyphRef /><linearGradient /><radialGradient /><textPath /></svg>"
    "Skip test tests11 <!DOCTYPE html><body><svg><altglyph /><altglyphdef /><altglyphitem /><animatecolor /><animatemotion /><animatetransform /><clippath /><feblend /><fecolormatrix /><fecomponenttransfer /><fecomposite /><feconvolvematrix /><fediffuselighting /><fedisplacementmap /><fedistantlight /><feflood /><fefunca /><fefuncb /><fefuncg /><fefuncr /><fegaussianblur /><feimage /><femerge /><femergenode /><femorphology /><feoffset /><fepointlight /><fespecularlighting /><fespotlight /><fetile /><feturbulence /><foreignobject /><glyphref /><lineargradient /><radialgradient /><textpath /></svg>"
    "Skip test tests11 <!DOCTYPE html><BODY><SVG><ALTGLYPH /><ALTGLYPHDEF /><ALTGLYPHITEM /><ANIMATECOLOR /><ANIMATEMOTION /><ANIMATETRANSFORM /><CLIPPATH /><FEBLEND /><FECOLORMATRIX /><FECOMPONENTTRANSFER /><FECOMPOSITE /><FECONVOLVEMATRIX /><FEDIFFUSELIGHTING /><FEDISPLACEMENTMAP /><FEDISTANTLIGHT /><FEFLOOD /><FEFUNCA /><FEFUNCB /><FEFUNCG /><FEFUNCR /><FEGAUSSIANBLUR /><FEIMAGE /><FEMERGE /><FEMERGENODE /><FEMORPHOLOGY /><FEOFFSET /><FEPOINTLIGHT /><FESPECULARLIGHTING /><FESPOTLIGHT /><FETILE /><FETURBULENCE /><FOREIGNOBJECT /><GLYPHREF /><LINEARGRADIENT /><RADIALGRADIENT /><TEXTPATH /></SVG>"
    "Skip test tests11 <!DOCTYPE html><body><math><altGlyph /><altGlyphDef /><altGlyphItem /><animateColor /><animateMotion /><animateTransform /><clipPath /><feBlend /><feColorMatrix /><feComponentTransfer /><feComposite /><feConvolveMatrix /><feDiffuseLighting /><feDisplacementMap /><feDistantLight /><feFlood /><feFuncA /><feFuncB /><feFuncG /><feFuncR /><feGaussianBlur /><feImage /><feMerge /><feMergeNode /><feMorphology /><feOffset /><fePointLight /><feSpecularLighting /><feSpotLight /><feTile /><feTurbulence /><foreignObject /><glyphRef /><linearGradient /><radialGradient /><textPath /></math>"
    "Skip test tests11 <!DOCTYPE html><body><svg><solidColor /></svg>"
    "Skip test tests15 <!doctype html></body><meta>"
    "Skip test tests15 <!doctype html><table> X<meta></table>"
    "Skip test tests16 <!doctype html><script><"
    "Skip test tests16 <!doctype html><script></"
    "Skip test tests16 <!doctype html><script></S"
    "Skip test tests16 <!doctype html><script></SC"
    "Skip test tests16 <!doctype html><script></SCR"
    "Skip test tests16 <!doctype html><script></SCRI"
    "Skip test tests16 <!doctype html><script></SCRIP"
    "Skip test tests16 <!doctype html><script></SCRIPT"
    "Skip test tests16 <!doctype html><script></s"
    "Skip test tests16 <!doctype html><script></sc"
    "Skip test tests16 <!doctype html><script></scr"
    "Skip test tests16 <!doctype html><script></scri"
    "Skip test tests16 <!doctype html><script></scrip"
    "Skip test tests16 <!doctype html><script></script"
    "Skip test tests16 <!doctype html><script><!--<"
    "Skip test tests16 <!doctype html><script><!--</"
    "Skip test tests16 <!doctype html><script><!--</script"
    "Skip test tests16 <!doctype html><script><!--<script </script <"
    "Skip test tests16 <!doctype html><script><!--<script </script </"
    "Skip test tests16 <!doctype html><script><!--<script </script </script"
    "Skip test tests16 <!doctype html><script><!--<script --><"
    "Skip test tests16 <!doctype html><script><!--<script --></"
    "Skip test tests16 <!doctype html><script><!--<script --></script"
    "Skip test tests16 <script><" "Skip test tests16 <script></"
    "Skip test tests16 <script></S" "Skip test tests16 <script></SC"
    "Skip test tests16 <script></SCR" "Skip test tests16 <script></SCRI"
    "Skip test tests16 <script></SCRIP" "Skip test tests16 <script></SCRIPT"
    "Skip test tests16 <script></s" "Skip test tests16 <script></sc"
    "Skip test tests16 <script></scr" "Skip test tests16 <script></scri"
    "Skip test tests16 <script></scrip" "Skip test tests16 <script></script"
    "Skip test tests16 <script><!--<" "Skip test tests16 <script><!--</"
    "Skip test tests16 <script><!--</script"
    "Skip test tests16 <script><!--<script </script <"
    "Skip test tests16 <script><!--<script </script </"
    "Skip test tests16 <script><!--<script </script </script"
    "Skip test tests16 <script><!--<script --><"
    "Skip test tests16 <script><!--<script --></"
    "Skip test tests16 <script><!--<script --></script"
    "Skip test tests19 <!doctype html><html><frameset></frameset></html>  "
    "Skip test tests19 <html> a <frameset></frameset>"
    "Skip test tests19 <!doctype html><meta charset=\"ascii\">"
    "Skip test tests19 <!doctype html><meta http-equiv=\"content-type\" content=\"text/html;charset=ascii\">"
    "Skip test tests19 <!doctype html><head><!--aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa--><meta charset=\"utf8\">"
    "Skip test tests2 <script></x"
    "Skip test tests2 <!doctypehtml><scrIPt type=text/x-foobar;baz>X</SCRipt"
    "Skip test tests2 &" "Skip test tests2 &#" "Skip test tests2 &#X"
    "Skip test tests2 &#x" "Skip test tests2 &#45" "Skip test tests2 &AMP"
    "Skip test tests2 <!DOCTYPE html><!-- X"
    "Skip test tests2 <!DOCTYPE html><!-- XXX - XXX"
    "Skip test tests2 <!DOCTYPE html><body><title>X</title><meta name=z><link rel=foo><style>
x { content:\"</style\" } </style>"
    "Skip test tests2  
 "
    "Skip test tests2 <!DOCTYPE html>  <html>"
    "Skip test tests2 <!DOCTYPE html><script>
</script>  <title>x</title>  </head>"
    "Skip test tests2 <!DOCTYPE html>X</html> "
    "Skip test tests2 <!DOCTYPE html><!--x--"
    "Skip test tests21 <svg><![CDATA[foo]]>"
    "Skip test tests21 <math><![CDATA[foo]]>"
    "Skip test tests21 <svg><![CDATA[foo" "Skip test tests21 <svg><![CDATA[foo"
    "Skip test tests21 <svg><![CDATA[]]>"
    "Skip test tests21 <svg><![CDATA[]] >]]>"
    "Skip test tests21 <svg><![CDATA[]] >]]>"
    "Skip test tests21 <svg><![CDATA[]]" "Skip test tests21 <svg><![CDATA[]"
    "Skip test tests21 <svg><![CDATA[]>a"
    "Skip test tests21 <!DOCTYPE html><svg><![CDATA[foo]]]]]>"
    "Skip test tests21 <svg><![CDATA[<svg>]]>"
    "Skip test tests21 <svg><![CDATA[</svg>a]]>"
    "Skip test tests21 <svg><![CDATA[<svg>a"
    "Skip test tests21 <svg><![CDATA[</svg>a"
    "Skip test tests21 <svg><![CDATA[<svg>]]><path>"
    "Skip test tests21 <svg><![CDATA[<svg>]]></path>"
    "Skip test tests21 <svg><![CDATA[<svg>]]><!--path-->"
    "Skip test tests21 <svg><![CDATA[<svg>]]>path"
    "Skip test tests21 <svg><![CDATA[<!--svg-->]]>"
    "Skip test tests25 <!DOCTYPE html><body><meta>A"
    "Skip test tests3 <!DOCTYPE html><html><head></head><body><pre>
</pre></body></html>"
    "Skip test tests3 <!DOCTYPE html><html><head></head><body><pre>
foo</pre></body></html>"
    "Skip test tests3 <!DOCTYPE html><html><head></head><body><pre>

foo</pre></body></html>"
    "Skip test tests3 <!DOCTYPE html><html><head></head><body><pre>
foo
</pre></body></html>"
    "Skip test tests3 <!DOCTYPE html><pre>&#x0a;&#x0a;A</pre>"
    "Skip test tests3 <!DOCTYPE html><HTML><META><HEAD></HEAD></HTML>"
    "Skip test tests3 <!DOCTYPE html><textarea>
</textarea>"
    "Skip test tests3 <!DOCTYPE html><textarea>
foo</textarea>"
    "Skip test tests3 <!DOCTYPE html><textarea>

foo</textarea>"
    "Skip test tests5 <style> <!-- </style> --> </style>x"
    "Skip test tests5 <script> <!-- </script> --> </script>x"
    "Skip test tests5 <title> <!-- </title> --> </title>x"
    "Skip test tests6 <!doctype html></head> <head>" "Skip test tests6 <!---x"
    "Skip test tests6 <frameset></frameset>
foo"
    "Skip test tests6 <frameset></frameset>
<noframes>"
    "Skip test tests6 <frameset></frameset>
<div>"
    "Skip test tests6 <frameset></frameset>
</html>"
    "Skip test tests6 <frameset></frameset>
</div>"
    "Skip test tests6 <html><frameset></frameset></html> "
    "Skip test tests7 <!doctype html><table><meta></table>"
    "Skip test tests7 <!doctype html><table>X<tr><td><table> <meta></table></table>"
    "Skip test tests7 <!doctype html><html> <head>"
    "Skip test tests7 <!doctype html> <head>"
    "Skip test tests7 <!doctype html><listing>
X</listing>"
    "Skip test tests9 <!DOCTYPE html><body xlink:href=foo xml:lang=en><math><mi xml:lang=en xlink:href=foo /></math>"
    "Skip test tests9 <!DOCTYPE html><body xlink:href=foo xml:lang=en><math><mi xml:lang=en xlink:href=foo />bar</math>"
    "Skip test tricky01 <html>
<font color=red><i>Italic and Red<p>Italic and Red </font> Just italic.</p> Italic only.</i> Plain
<p>I should not be red. <font color=red>Red. <i>Italic and red.</p>
<p>Italic and red. </i> Red.</font> I should not be red.</p>
<b>Bold <i>Bold and italic</b> Only Italic </i> Plain"
    "Skip test tricky01 <html>
<dl>
<dt><b>Boo
<dd>Goo?
</dl>
</html>"
    "Skip test tricky01 <html>
<body>
<b><nobr><div>This text is in a div inside a nobr</nobr>More text that should not be in the nobr, i.e., the
nobr should have closed the div inside it implicitly. </b><pre>A pre tag outside everything else.</pre>
</body>
</html>"
    "Skip test webkit01 <head></head>
<body></body>"
    "Skip test webkit01 <head></head> <style></style>ddd"
    "Skip test webkit01 <font></p><p><meta><title></title></font>"
    ))

(defun run-html5-parser-tests ()
  (setf *skipped-errors* nil)
  (handler-bind ((error (lambda (e)
                          (declare (ignore e))
                          (when (find-restart 'skip)
                            (when (member (princ-to-string (find-restart 'skip))
                                          *known-failures*
                                          :test #'string=)
                              (invoke-restart 'skip))
                            (when *skip-all-errors*
                              (pushnew (princ-to-string (find-restart 'skip)) *skipped-errors*)
                              (invoke-restart 'skip))))))
    (values (test-tokenizer)
            (test-parser)
            (run! 'html5-parser-tests)
            )))
