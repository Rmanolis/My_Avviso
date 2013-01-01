package code.lib
import net.liftweb._
import common._
import http.S
import sitemap._
import sitemap.Loc._
import code.model._

case class MenuLoc(menu: Menu) {
  lazy val url: String = S.contextPath + menu.loc.calcDefaultHref
  lazy val fullUrl: String = S.hostAndPath + menu.loc.calcDefaultHref
}

object MenuGroups {
  val SettingsGroup = LocGroup("settings")
  val TopBarGroup = LocGroup("topbar")

}
object Site {
  import MenuGroups._

  val home = MenuLoc(Menu.i("Home") / "index")
  val isLoggedIn = If(() => User.loggedIn_?, "You must be logged in")
  val isAdminLoggedIn = If(() => User.loggedIn_? && User.currentUser.map {
    u => u.superUser.is
  }.getOrElse(false), "You must be logged in")

  val addArticle = MenuLoc(Menu.i("Add Article") / "admin" / "set" / "addArticle" >> isLoggedIn)
  private val editArticleMenu = Menu.param[Article]("EditArticle", "EditArticle",
    Article.find _, _.id.is.toString) / "admin" / "set" / "editArticle" >> isLoggedIn >> Hidden
  lazy val editArticleLoc = editArticleMenu.toLoc

  val addCategory = MenuLoc(Menu.i("Add Category") / "admin" / "set" / "addCategory" >> isAdminLoggedIn)
  private val editCategoryMenu = Menu.param[Category]("Category", "Edit Category",
    Category.find _, _.id.is.toString) / "admin" / "set" / "editCategory" >> isAdminLoggedIn >> Hidden
  lazy val editCategoryLoc = editCategoryMenu.toLoc

  val addLanguage = MenuLoc(Menu.i("Add Language") / "admin" / "set" / "addLanguage" >> isAdminLoggedIn)
  private val editLanguageMenu = Menu.param[Language]("Language", "Edit Language",
    Language.find _, _.id.is.toString) / "admin" / "set" / "editLanguage" >> isAdminLoggedIn >> Hidden
  lazy val editLanguageLoc = editLanguageMenu.toLoc

  val addLocalizedLabel = MenuLoc(Menu.i("Add Localized Label") / "admin" / "set" / "addLocalizedLabel" >> isAdminLoggedIn)
  private val editLocalizedLabelMenu = Menu.param[Localization]("LocalizedLabel", "Edit LocalizedLabel",
    Localization.find _, _.id.is.toString) / "admin" / "set" / "editLocalizedLabel" >> isAdminLoggedIn >> Hidden
  lazy val editLocalizedLabelLoc = editLocalizedLabelMenu.toLoc

  val addAdvert = MenuLoc(Menu.i("Add Advert") / "admin" / "set" / "addAdvert" >> isAdminLoggedIn)
  private val editAdvertMenu = Menu.param[Advert]("Advert", "Edit Advert",
    Advert.find _, _.id.is.toString) / "admin" / "set" / "editAdvert" >> isAdminLoggedIn >> Hidden
  lazy val editAdvertLoc = editAdvertMenu.toLoc

  val addFile = MenuLoc(Menu.i("Upload File") / "admin" / "set" / "addFile" >> isLoggedIn)

  private val addArticleFileMenu = Menu.param[Article]("Article File", "Upload File to the Article ",
    Article.find _, _.id.is.toString) / "admin" / "set" / "addArticleFile" >> isLoggedIn >> Hidden
  lazy val addArticleFileLoc = addArticleFileMenu.toLoc

  private val addUser = MenuLoc(Menu.i("Add User") / "admin" / "set" / "addUser" >> isAdminLoggedIn)

  val crudLanguages = MenuLoc(Menu.i("Languages") / "admin" / "languages" >> isAdminLoggedIn)
  val crudCategories = MenuLoc(Menu.i("Categories") / "admin" / "categories" >> isAdminLoggedIn)
  val crudLocalizedLabels = MenuLoc(Menu.i("Localized Labels") / "admin" / "localizedLabels" >> isAdminLoggedIn)
  val crudAdverts = MenuLoc(Menu.i("Adverts") / "admin" / "adverts" >> isAdminLoggedIn)
  val crudUsers = MenuLoc(Menu.i("Users") / "admin" / "users" >> isAdminLoggedIn)
  val crudArticles = MenuLoc(Menu.i("Articles") / "admin" / "articles" >> isLoggedIn)
  val crudFiles = MenuLoc(Menu.i("Files") / "admin" / "files" >> isLoggedIn)

  private val chooseCategoryMenu = Menu.param[Category]("ShowCategory", "Show Category",
    Category.findByName _, _.name.is) / "category" >> Hidden
  lazy val chooseCategoryLoc = chooseCategoryMenu.toLoc

  private val chooseArticleMenu = Menu.param[Article]("ShowArticle", "Show Article",
    Article.find _, _.id.is.toString) / "article" >> Hidden
  lazy val chooseArticleLoc = chooseArticleMenu.toLoc

  private def menu = List(
    home.menu,
    User.loginMenuLoc.get,
    User.logoutMenuLoc.get,
    User.editUserMenuLoc.get,
    User.changePasswordMenuLoc.get,
    addArticle.menu,
    crudArticles.menu,
    addLanguage.menu,
    crudLanguages.menu,
    addCategory.menu,
    crudCategories.menu,
    addLocalizedLabel.menu,
    crudLocalizedLabels.menu,
    addAdvert.menu,
    crudAdverts.menu,
    addFile.menu,
    crudFiles.menu,
    addArticleFileMenu,
    editArticleMenu,
    editCategoryMenu,
    editLanguageMenu,
    editLocalizedLabelMenu,
    editAdvertMenu,
    chooseCategoryMenu,
    chooseArticleMenu,
    addUser.menu,
    crudUsers.menu)

  def siteMap = SiteMap(menu: _*)
}