import os
import time
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from bs4 import BeautifulSoup

# 保存先ディレクトリ
SAVE_DIR = "html_data_all"
os.makedirs(SAVE_DIR, exist_ok=True)

# 東京都23区と対応するSUUMOコード（scパラメータ）
WARD_LIST = [
    ("千代田区", "13101"), ("中央区", "13102"), ("港区", "13103"),
    ("新宿区", "13104"), ("文京区", "13105"), ("台東区", "13106"),
    ("墨田区", "13107"), ("江東区", "13108"), ("品川区", "13109"),
    ("目黒区", "13110"), ("大田区", "13111"), ("世田谷区", "13112"),
    ("渋谷区", "13113"), ("中野区", "13114"), ("杉並区", "13115"),
    ("豊島区", "13116"), ("北区", "13117"), ("荒川区", "13118"),
    ("板橋区", "13119"), ("練馬区", "13120"), ("足立区", "13121"),
    ("葛飾区", "13122"), ("江戸川区", "13123")
]

# 共通クエリパラメータ
BASE_URL = "https://suumo.jp/jj/bukken/ichiran/JJ012FC001/"
COMMON_QUERY = (
    "?jj012fi20202Kbn=3"
    "&initKbn=1"
    "&displayClass=dn"
    "&disabledClass=false"
    "&ar=030"
    "&bs=011"
    "&ta=13"
    "&cn=9999999"
    "&kb=1"
    "&kt=9999999"
    "&mb=0"
    "&mt=9999999"
    "&md=3"
    "&md=4"
    "&md=5"
    "&et=9999999"
    "&et=9999999"
    "&fw2="
)
WAIT_TIME = 2

def init_driver() -> webdriver.Chrome:
    """
    ヘッドレスChromeドライバーを初期化する。

    Returns:
        webdriver.Chrome: 初期化されたChromeドライバーオブジェクト
    """
    opts = Options()
    opts.add_argument("--headless")
    opts.add_argument("--disable-gpu")
    return webdriver.Chrome(options=opts)

def crawl_ward(driver: webdriver.Chrome, ward_name: str, sc_code: str) -> None:
    """
    指定した区の物件一覧をSUUMOからすべて取得し、HTMLを保存する。

    Args:
        driver (webdriver.Chrome): SeleniumのChromeドライバー
        ward_name (str): 区の名前（例: "港区"）
        sc_code (str): 対応するSUUMOのscパラメータ（例: "13103"）

    Returns:
        None
    """
    page = 1
    while True:
        # ページ1はCOMMON_QUERYのみ、それ以降は &page=
        url = f"{BASE_URL}{COMMON_QUERY}&sc={sc_code}&scTemp={sc_code}&page={page if page>1 else ''}"
        print(f"{ward_name} ページ{page} にアクセス: {url}")
        driver.get(url)
        time.sleep(WAIT_TIME)
        html = driver.page_source
        soup = BeautifulSoup(html, "html.parser")
        listings = soup.select("div.property_unit")
        if not listings:
            print(f"{ward_name} ページ{page} に物件なし → 終了")
            break
        filename = f"{ward_name}_{page}.html"
        with open(os.path.join(SAVE_DIR, filename), "w", encoding="utf-8") as f:
            f.write(html)
        print(f"保存: {filename}")
        page += 1

def main():
    """
    各区のSUUMO物件情報をクロールし、HTMLとして保存する。
    """
    driver = init_driver()
    try:
        for ward_name, sc_code in WARD_LIST:
            crawl_ward(driver, ward_name, sc_code)
    finally:
        driver.quit()
        print("完了：ドライバ終了")

if __name__ == "__main__":
    main()